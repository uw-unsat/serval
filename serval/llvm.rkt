#lang rosette/safe

(require (prefix-in core: "lib/core.rkt")
         (prefix-in ubsan: "ubsan.rkt"))
(require (only-in racket/base make-parameter symbol->string string->symbol substring))
(require rosette/lib/match)
(require (prefix-in base: racket/base))

(provide (except-out (all-defined-out) @bug-on))


; machine

(struct frame (pc label predecessor allocas) #:mutable #:transparent)

(struct machine (frames mregions retval merge-stack merge-point) #:mutable #:transparent)

(define (make-empty-frame)
  (frame #f #f #f null))

(define (make-machine [symbols null] [globals null])
  (define mregions (core:create-mregions symbols globals))
  (define-symbolic* %0 (bitvector 64))
  (machine (list (make-empty-frame)) mregions %0 null #f))

(define current-machine (make-parameter #f))

(define (current-frames)
  (machine-frames (current-machine)))

(define (current-mregions)
  (machine-mregions (current-machine)))

(define (current-frame)
  (car (current-frames)))

(define (current-pc)
  (frame-pc (current-frame)))

(define (current-merge-stack)
  (machine-merge-stack (current-machine)))

(define (current-merge-point)
  (machine-merge-point (current-machine)))

(define (@bug-on v msg)
  (core:bug-on v #:key 'llvm #:dbg (current-pc) #:msg msg))


; control flow

(define (unreachable) (@bug-on #t "unreachable"))

(define (enter! label)
  (define frame (current-frame))
  (set-frame-predecessor! frame (frame-label frame))
  (set-frame-label! frame label)
  (label))


(struct function (proc args dbg)
  #:transparent
  #:property prop:procedure (struct-field-index proc))


; Use lambda to delay the evaluation of dbg.
(define-syntax (define-function stx)
  (syntax-case stx ()
    [(_ (name [arg-id : arg-type] ...) body ...)
     (syntax/loc stx
       (define name
         (function (lambda (arg-id ...) body ...)
                   (list (cons (substring (symbol->string 'arg-id) 1) arg-type) ...)
                   'name)))]))


(define-syntax (define-label stx)
  (syntax-case stx ()
    [(_ (x) #:merge y instr ...)
      #'(define (x)
        (define name (quote x))
        (define stack (current-merge-stack))
        (if (base:and (not (null? stack)) (equal? (car stack) name))
          (begin (set-machine-merge-stack! (current-machine) (cdr stack)))
          (begin
            (set-machine-merge-point! (current-machine) (cons y (quote y)))
            (begin
              (set-frame-pc! (current-frame) 'instr)
              instr) ...)))]))

(define-syntax-rule (define-value x)
  (define x #f))

(define-syntax (phi stx)
  (syntax-case stx ()
    [(_ [val label] ...)
     (syntax/loc stx
       (cond
         [(equal? label (frame-predecessor (current-frame))) val] ...))]))

(define-syntax (switch stx)
  (syntax-case stx ()
    [(_ val default-label [other-val other-label] ...)
     (syntax/loc stx
       (let ([v val])
         (cond
           [(equal? v other-val) (enter! other-label)] ...
           [else (enter! default-label)])))]))

(define asm
  (lambda (opcode . args)
    (case opcode
      [(nop)
       (match args
         [(list) (void)]  ; for unreachable
         [(list val) val] ; for array_size_nospec
         [_ (core:bug-on #t #:msg "asm: unknown nop")])]
      [else (core:bug-on #t #:msg "asm: not supported")])))

(define (call func . args)
  (define mach (current-machine))
  (define frames (current-frames))
  (define fresh (make-empty-frame))
  ; make fresh the new frame
  (set-machine-frames! mach (cons fresh frames))
  ; invoke the function
  (define result (apply func args))
  ; reset the frames
  (set-machine-frames! mach frames)
  ; free stack memory
  (for-each core:mblock-fini! (frame-allocas fresh))
  result)

(define ret
  (case-lambda
    [() (void)]
    [(x) x]))

(define br
  (case-lambda
    [(succ) (enter! succ)]
    [(test succ-true succ-false)
      (define mergepoint (current-merge-point))
      (define stack (current-merge-stack))

      (if (car mergepoint)
        (begin
          (set-machine-merge-stack! (current-machine) (cons (cdr mergepoint) stack))
          (if (core:bitvector->bool test)
            (enter! succ-true)
            (enter! succ-false))
          ((car mergepoint)))
        (begin
          (enter! (if (core:bitvector->bool test) succ-true succ-false))))]))

; undef

(define (undef type)
  (cond
    [(list? type)
     (map undef type)]
    [(vector? type)
     (list->vector (map undef (vector->list type)))]
    [else
     (define-symbolic* symbolic-undef type)
     symbolic-undef]))


; comparisons

(define (icmp/eq x y)
  (core:bool->bitvector (bveq x y)))

(define (icmp/ne x y)
  (core:bool->bitvector (not (bveq x y))))

(define (icmp/ugt x y)
  (core:bool->bitvector (bvugt x y)))

(define (icmp/uge x y)
  (core:bool->bitvector (bvuge x y)))

(define (icmp/ult x y)
  (core:bool->bitvector (bvult x y)))

(define (icmp/ule x y)
  (core:bool->bitvector (bvule x y)))

(define (icmp/sgt x y)
  (core:bool->bitvector (bvsgt x y)))

(define (icmp/sge x y)
  (core:bool->bitvector (bvsge x y)))

(define (icmp/slt x y)
  (core:bool->bitvector (bvslt x y)))

(define (icmp/sle x y)
  (core:bool->bitvector (bvsle x y)))

(define (select cond x y)
  (if (core:bitvector->bool cond) x y))


; arithmetic operations

; We are not checking flags such as nsw, nuw, and exact - they should be
; removed from the IR before code generation, and we verify the final
; instructions anyway.

(define add bvadd)

(define sub bvsub)

(define mul bvmul)

(define (udiv x y)
  (@bug-on (core:bvzero? y) "udiv: division by zero")
  (bvudiv x y))

(define (urem x y)
  (@bug-on (core:bvzero? y) "urem: division by zero")
  (bvurem x y))

(define (sdiv x y)
  (@bug-on (core:bvzero? y) "sdiv: division by zero")
  (@bug-on (core:bvsdiv-overflow? x y) "sdiv: signed division overflow")
  (bvsdiv x y))

(define (srem x y)
  (@bug-on (core:bvzero? y) "srem: division by zero")
  (@bug-on (core:bvsdiv-overflow? x y) "srem: signed division overflow")
  (bvsrem x y))


; logical operations

(define (shl x y)
  (@bug-on (bvuge y (bv (core:bv-size x) (type-of y)))
           "shl: shift too large")
  (bvshl x y))

(define (lshr x y)
  (@bug-on (bvuge y (bv (core:bv-size x) (type-of y)))
           "lshr: shift too large")
  (bvlshr x y))

(define (ashr x y)
  (@bug-on (bvuge y (bv (core:bv-size x) (type-of y)))
           "ashr: shift too large")
  (bvashr x y))

(define and bvand)

(define or bvor)

(define xor bvxor)


; casts

(define sext sign-extend)

(define zext zero-extend)

(define (trunc x type)
  (extract (sub1 (bitvector-size type)) 0 x))

; ignore pointer casts
(define (bitcast x) x)

(define (inttoptr x)
  (core:bug-on #t #:msg "inttoptr: not supported"))

(define (ptrtoint ptr type)
  (define mr (core:find-mregion-by-name (current-mregions) (core:pointer-base ptr)))
  (@bug-on (not mr) (format "ptrtoint: mregion not found: ~a" ptr))
  (define start (core:bvpointer (core:mregion-start mr)))
  (bvadd start (core:pointer-offset ptr)))


; vector operations
; TBD

; aggregate operations

; immutable (returns a new aggregate value)
(define (insertvalue val elt . indices)
  (if (empty? indices)
      elt
      (let ([pos (car indices)])
        (define-values (lhs rhs) (split-at val pos))
        ; recursively call insertvalue with the element at pos
        (append lhs (cons (apply insertvalue (car rhs) elt (cdr indices)) (cdr rhs))))))

(define (extractvalue val . indices)
  (if (empty? indices)
      val
      (apply extractvalue (list-ref val (car indices)) (cdr indices))))


; memory operations

(define (pointer-block ptr)
  (define base (core:pointer-base ptr))
  (if (core:mblock? base)
      base
      (symbol->block base)))

(define (symbol->block sym)
  (core:mregion-block
    (core:find-mregion-by-name (current-mregions) sym)))

; LLVM prefixes global names with '@'
(define (global->symbol s)
  (string->symbol (substring (symbol->string s) 1)))

(define-syntax-rule (define-global x)
  (define x (core:pointer (global->symbol 'x) (core:bvpointer 0))))

(define (array-offset index size)
  ; sign-extend index as it may be 32-bit
  (bvmul (sign-extend index (core:bvpointer?)) (core:bvpointer size)))

(define (struct-offset size)
  (core:bvpointer size))

(define (getelementptr ptr . offsets)
  (core:pointer (core:pointer-base ptr) (apply bvadd (cons (core:pointer-offset ptr) offsets))))

(define (load ptr type #:align alignment)
  (define mblock (pointer-block ptr))
  (define offset (core:pointer-offset ptr))
  (define size (core:bvpointer (/ (bitvector-size type) 8)))
  (define path (core:mblock-path mblock offset size #:dbg (current-pc)))
  (core:spectre-bug-on (not (core:mblock-inbounds? mblock offset size)) #:dbg (current-pc)
   #:msg (format "spectre: load @ ~a\n" ptr))
  (core:mblock-iload mblock path))

(define (store value ptr type #:align alignment)
  (define mblock (pointer-block ptr))
  (define offset (core:pointer-offset ptr))
  (define size (core:bvpointer (/ (bitvector-size type) 8)))
  (define path (core:mblock-path mblock offset size #:dbg (current-pc)))
  (core:mblock-istore! mblock value path))

; alloca allocates a stack pointer; mblock is uninitialized.
(define (alloca block #:align alignment)
  ; add the block to the allocas list of the current frame
  (define frame (current-frame))
  (set-frame-allocas! frame (cons block (frame-allocas frame)))
  (core:pointer block (core:bvpointer 0)))

(define (memset ptr c size)
  (define mblock (pointer-block ptr))
  (define offset (core:pointer-offset ptr))
  (core:mblock-memset! mblock (extract 7 0 c) offset size #:dbg (current-pc))
  ptr)


; builtin calls

(define (llvm.sadd.with.overflow x y)
  (list (bvadd x y)
        (core:bool->bitvector (core:bvsadd-overflow? x y))))

(define llvm.sadd.with.overflow.i16 llvm.sadd.with.overflow)
(define llvm.sadd.with.overflow.i32 llvm.sadd.with.overflow)
(define llvm.sadd.with.overflow.i64 llvm.sadd.with.overflow)

(define (llvm.uadd.with.overflow x y)
  (list (bvadd x y)
        (core:bool->bitvector (core:bvuadd-overflow? x y))))

(define llvm.uadd.with.overflow.i16 llvm.uadd.with.overflow)
(define llvm.uadd.with.overflow.i32 llvm.uadd.with.overflow)
(define llvm.uadd.with.overflow.i64 llvm.uadd.with.overflow)

(define (llvm.ssub.with.overflow x y)
  (list (bvsub x y)
        (core:bool->bitvector (core:bvssub-overflow? x y))))

(define llvm.ssub.with.overflow.i16 llvm.ssub.with.overflow)
(define llvm.ssub.with.overflow.i32 llvm.ssub.with.overflow)
(define llvm.ssub.with.overflow.i64 llvm.ssub.with.overflow)

(define (llvm.usub.with.overflow x y)
  (list (bvsub x y)
        (core:bool->bitvector (core:bvusub-overflow? x y))))

(define llvm.usub.with.overflow.i16 llvm.usub.with.overflow)
(define llvm.usub.with.overflow.i32 llvm.usub.with.overflow)
(define llvm.usub.with.overflow.i64 llvm.usub.with.overflow)

(define (llvm.smul.with.overflow x y)
  (list (bvmul x y)
        (core:bool->bitvector (core:bvsmul-overflow? x y))))

(define llvm.smul.with.overflow.i16 llvm.smul.with.overflow)
(define llvm.smul.with.overflow.i32 llvm.smul.with.overflow)
(define llvm.smul.with.overflow.i64 llvm.smul.with.overflow)

(define (llvm.umul.with.overflow x y)
  (list (bvmul x y)
        (core:bool->bitvector (core:bvumul-overflow? x y))))

(define llvm.umul.with.overflow.i16 llvm.umul.with.overflow)
(define llvm.umul.with.overflow.i32 llvm.umul.with.overflow)
(define llvm.umul.with.overflow.i64 llvm.umul.with.overflow)

(define (llvm.bswap.i16 x)
  (concat (extract 7 0 x) (extract 15 8 x)))

(define (llvm.bswap.i32 x)
  (concat (llvm.bswap.i16 (extract 15 0 x)) (llvm.bswap.i16 (extract 31 16 x))))

(define (llvm.bswap.i64 x)
  (concat (llvm.bswap.i32 (extract 31 0 x)) (llvm.bswap.i32 (extract 63 32 x))))

; Don't do anything.  This leaves mcells as #f: any read before write
; will trigger undefined behavior.
(define (llvm.lifetime.start.p0i8 size ptr)
  (void))

; the lifetime of stack memory is handled by alloca and call;
; lifetime pairs may not be well-formed.
(define (llvm.lifetime.end.p0i8 size ptr)
  (void))

(define (llvm.memset.p0i8.i64 ptr c size isvolatile)
  (memset ptr c size)
  (void))

(define (memzero_explicit ptr size)
  (memset ptr (bv 0 32) size)
  (void))


; assertions

; Linux
(define (__assert_fail expr filename line func)
  (core:bug-on #t #:key 'assert #:dbg (ubsan:source-location filename line #f) #:msg expr))

; unknown
(define (__assert_func filename line func expr)
  (__assert_fail expr filename line func))

; macOS
(define (__assert_rtn func filename line expr)
  (__assert_fail expr filename line func))
