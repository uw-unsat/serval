#lang rosette

(require
  (prefix-in core: "lib/core.rkt")
  rosette/base/core/polymorphic)

(provide (all-defined-out))


(define bpf-verbose (make-parameter #f))
(define bpf-allow-leak-pointer (make-parameter #f))
(define bpf-strict-pointer (make-parameter #f))


(struct insn (code dst src off imm) #:transparent)


; strict check: allow a pointer to be from 0 to one past the end of a block
; thus offset <= size (rather than offset < size)
(define (bpf-pointer base offset)
  (when (bpf-strict-pointer)
    (let ([size (core:mblock-size base)])
      (core:bug-on (bvugt offset (bv size (core:bv-size offset))) #:dbg current-pc-debug
       #:msg (format "invalid pointer offset out of block size ~e: ~e\n" size offset))))
  (core:pointer base offset))


; maps

(define-generics bpf-map
  (bpf-map-key-size bpf-map)
  (bpf-map-value-size bpf-map)
  (bpf-map-num-entries bpf-map)
  (bpf-map-lookup bpf-map key-ptr))

; array-map

(define (array-map-lookup bm keyp)
  (define num-entries (bpf-map-num-entries bm))
  (define value-size (bpf-map-value-size bm))
  (define index (list->bitvector (load-bytes keyp (bpf-map-key-size bm))))
  (define block (array-map-block bm))
  (define offset (bvmul (zero-extend index (bitvector 64))
                        (bv value-size 64)))
  ; null if out of bounds
  (if (bvuge index (bv num-entries (core:bv-size index)))
      (bv 0 64)
      (bpf-pointer block offset)))

(struct array-map (value-size num-entries [block #:mutable]) #:transparent
  #:methods gen:bpf-map
  [(define bpf-map-key-size (lambda (m) 4))
   (define (bpf-map-value-size bm) (array-map-value-size bm))
   (define (bpf-map-num-entries bm) (array-map-num-entries bm))
   (define bpf-map-lookup array-map-lookup)])

(define (make-array-map value-size num-entries)
  (define block (core:marray (* value-size num-entries) (core:mcell 1)))
  ; make the map symbolic as user space might write to it
  ; in fact we could even use a "volatile" block
  (core:mblock-init! block (list))
  (array-map value-size num-entries block))


; register numbers

(define BPF_REG_0  0)
(define BPF_REG_1  1)
(define BPF_REG_2  2)
(define BPF_REG_3  3)
(define BPF_REG_4  4)
(define BPF_REG_5  5)
(define BPF_REG_6  6)
(define BPF_REG_7  7)
(define BPF_REG_8  8)
(define BPF_REG_9  9)
(define BPF_REG_10 10)
(define MAX_BPF_REG 11)

; ArgX, context and stack frame pointer register positions. Note,
; Arg1, Arg2, Arg3, etc are used as argument mappings of function
; calls in BPF_CALL instruction.

(define BPF_REG_ARG1    BPF_REG_1)
(define BPF_REG_ARG2    BPF_REG_2)
(define BPF_REG_ARG3    BPF_REG_3)
(define BPF_REG_ARG4    BPF_REG_4)
(define BPF_REG_ARG5    BPF_REG_5)
(define BPF_REG_CTX     BPF_REG_6)
(define BPF_REG_FP      BPF_REG_10)

; Additional register mappings for converted user programs.
(define BPF_REG_A       BPF_REG_0)
(define BPF_REG_X       BPF_REG_7)
(define BPF_REG_TMP     BPF_REG_8)

; BPF program can access up to 512 bytes of stack space.

(define MAX_BPF_STACK   512)

(define BPF_PSEUDO_MAP_FD 1)


;  helper functions

(define BPF_FUNC_unspec          0)
(define BPF_FUNC_map_lookup_elem 1)
(define BPF_FUNC_map_update_elem 2)
(define BPF_FUNC_map_delete_elem 3)


; ALU ops on registers, bpf_add|sub|...: dst_reg += src_reg

(define-syntax-rule (BPF_ALU64_REG OP DST SRC)
  (make-insn '(BPF_ALU64 OP BPF_X) DST SRC #f #f))

(define-syntax-rule (BPF_ALU32_REG OP DST SRC)
  (make-insn '(BPF_ALU OP BPF_X) DST SRC #f #f))


; ALU ops on immediates, bpf_add|sub|...: dst_reg += imm32

(define-syntax-rule (BPF_ALU64_IMM OP DST IMM)
  (case 'OP
    [(BPF_NEG) (make-insn '(BPF_ALU64 OP) DST #f #f #f)]
    [else (make-insn '(BPF_ALU64 OP BPF_K) DST #f #f IMM)]))

(define-syntax-rule (BPF_ALU32_IMM OP DST IMM)
  (case 'OP
    [(BPF_NEG) (make-insn '(BPF_ALU OP) DST #f #f #f)]
    [else (make-insn '(BPF_ALU OP BPF_K) DST #f #f IMM)]))

; Endianess conversion, cpu_to_{l,b}e(), {l,b}e_to_cpu()

(define-syntax-rule (BPF_ENDIAN TYPE DST LEN)
  (make-insn '(BPF_ALU BPF_END TYPE) DST #f #f LEN))


; Short form of mov, dst_reg = src_reg

(define (BPF_MOV64_REG DST SRC)
  (make-insn '(BPF_ALU64 BPF_MOV BPF_X) DST SRC #f #f))

(define (BPF_MOV32_REG DST SRC)
  (make-insn '(BPF_ALU BPF_MOV BPF_X) DST SRC #f #f))


; Short form of mov, dst_reg = imm32

(define (BPF_MOV64_IMM DST IMM)
  (make-insn '(BPF_ALU64 BPF_MOV BPF_K) DST #f #f IMM))

(define (BPF_MOV32_IMM DST IMM)
  (make-insn '(BPF_ALU BPF_MOV BPF_K) DST #f #f IMM))


; BPF_LD_IMM64 macro encodes single 'load 64-bit immediate' insn

(define (BPF_LD_IMM64 DST IMM)
  (BPF_LD_IMM64_RAW DST #f IMM))

(define (BPF_LD_IMM64_RAW DST SRC IMM)
  (list (make-insn '(BPF_LD BPF_DW BPF_IMM) DST SRC #f (bitwise-and IMM #xffffffff))
        (make-insn #f #f #f #f (arithmetic-shift IMM -32))))

; pseudo BPF_LD_IMM64 insn used to refer to process-local map_fd
(define (BPF_LD_MAP_FD DST MAP_FD)
  (BPF_LD_IMM64_RAW DST BPF_PSEUDO_MAP_FD MAP_FD))


; Memory load, dst_reg = *(uint *) (src_reg + off16)

(define-syntax-rule (BPF_LDX_MEM SIZE DST SRC OFF)
  (make-insn '(BPF_LDX SIZE BPF_MEM) DST SRC OFF #f))

; Memory store, *(uint *) (dst_reg + off16) = src_reg

(define-syntax-rule (BPF_STX_MEM SIZE DST SRC OFF)
  (make-insn '(BPF_STX SIZE BPF_MEM) DST SRC OFF #f))


; Atomic memory add, *(uint *)(dst_reg + off16) += src_reg

(define-syntax-rule (BPF_STX_XADD SIZE DST SRC OFF)
  (make-insn '(BPF_STX SIZE BPF_XADD) DST SRC OFF #f))


; Memory store, *(uint *) (dst_reg + off16) = imm32

(define-syntax-rule (BPF_ST_MEM SIZE DST OFF IMM)
  (make-insn '(BPF_ST SIZE BPF_MEM) DST #f OFF IMM))


; Conditional jumps against registers, if (dst_reg 'op' src_reg) goto pc + off16

(define-syntax-rule (BPF_JMP_REG OP DST SRC OFF)
  (make-insn '(BPF_JMP OP BPF_X) DST SRC OFF #f))


; Conditional jumps against immediates, if (dst_reg 'op' imm32) goto pc + off16

(define-syntax-rule (BPF_JMP_IMM OP DST IMM OFF)
  (make-insn '(BPF_JMP OP BPF_K) DST #f OFF IMM))


; Function call

(define (BPF_EMIT_CALL FUNC)
  (make-insn '(BPF_JMP BPF_CALL) #f #f #f FUNC))


; Program exit

(define (BPF_EXIT_INSN)
  (make-insn '(BPF_JMP BPF_EXIT) #f #f #f #f))


; Debugging

(define (BPF_TRACE LOC)
  (make-insn '(BPF_TRACE) #f #f #f LOC))


(define (make-insn code dst src off imm)
  (insn code dst src
        (if (boolean? off) off (bv off 16))
        (if (boolean? imm) imm (bv imm 32))))

(define make-insns
  (lambda lst
    (define insns (flatten lst))
    (define keys (build-list (length insns) (lambda (x) (bv x 64))))
    (make-immutable-hash (map cons keys insns))))


(define current-pc-debug #f)

(struct cpu (pc regs fdtable seen) #:mutable #:transparent
  #:methods gen:custom-write
  [(define (write-proc cpu port mode)
     (define regs (cpu-regs cpu))
     (define fdtable (cpu-fdtable cpu))
     (fprintf port "(cpu")
     (fprintf port "\n  pc . ~a" (cpu-pc cpu))
     (for ([i (in-range (vector-length regs))])
       (fprintf port "\n  r~a . ~a" i (vector-ref regs i)))
     (for ([i (in-range (vector-length fdtable))])
       (fprintf port "\n  fd~a . ~a" i (vector-ref fdtable i)))
     (fprintf port ")"))])

(define (init-cpu [ctx #f] [fdtable (vector)])
  ; initially regs are uninitialized and must be written before read
  (define regs (make-vector MAX_BPF_REG #f))
  ; R1 points to context
  (vector-set! regs BPF_REG_1 ctx)
  ; R10 points to the stack, which is uninitialized
  (define stack (core:marray MAX_BPF_STACK (core:mcell 1)))
  (vector-set! regs BPF_REG_FP (bpf-pointer stack (bv MAX_BPF_STACK 64)))
  (cpu (bv 0 64) regs fdtable null))


(define (update-seen! cpu instructions pc)
  (define seen (cpu-seen cpu))
  (set-cpu-seen! cpu (cons pc seen))
  (set! current-pc-debug (bitvector->natural pc))
  (verbose "~a" (cons current-pc-debug (hash-ref instructions pc))))


(define (reg-set! cpu reg val)
  (core:bug-on (! (|| (bv? val) (core:pointer? val)))
   #:msg (format "reg-set!: not a bitvector/pointer: ~e" val) #:dbg current-pc-debug)
  (core:bug-on (equal? reg BPF_REG_10)
   #:msg (format "reg-set!: R10 is read-only") #:dbg current-pc-debug)
  (vector-set! (cpu-regs cpu) reg val))

(define (reg-havoc! cpu reg)
  (core:bug-on (equal? reg BPF_REG_10)
   #:msg (format "reg-havoc!: R10 is read-only") #:dbg current-pc-debug)
  (vector-set! (cpu-regs cpu) reg #f))

(define (reg-ref cpu reg)
  (define val (vector-ref (cpu-regs cpu) reg))
  (core:bug-on (boolean? val) #:dbg current-pc-debug
   #:msg (format "reg-ref: uninitialized register: ~e" reg))
  val)

(define (evaluate-alu64 op v1 v2)
  (case op
    [(BPF_ADD)
     (cond
       [(bv? v1)
        (bvadd v1 v2)]
       [(core:pointer? v1)
        (bpf-pointer (core:pointer-base v1) (bvadd (core:pointer-offset v1) v2))]
       [else (core:bug-on #t #:dbg current-pc-debug
              #:msg (format "unknown BPF_ADD operands: ~e ~e\n" v1 v2))])]
    [(BPF_SUB)
     (cond
       [(&& (bv? v1) (bv? v2))
        (bvsub v1 v2)]
       [(&& (core:pointer? v1) (bv? v2))
        (bpf-pointer (core:pointer-base v1) (bvsub (core:pointer-offset v1) v2))]
       [(&& (core:pointer? v1) (core:pointer? v2))
        (core:bug-on (! (equal? (core:pointer-base v1) (core:pointer-base v2))) #:dbg current-pc-debug
         #:msg (format "BPF_SUB: pointers of different blocks: ~e ~e\n" v1 v2))
        (bvsub (core:pointer-offset v1) (core:pointer-offset v2))]
       [else (core:bug-on #t #:dbg current-pc-debug
              #:msg (format "unknown BPF_SUB operands: ~e ~e\n" v1 v2))])]
    [(BPF_MUL) (bvmul v1 v2)]
    [(BPF_DIV)
     (core:bug-on (core:bvzero? v2) #:dbg current-pc-debug
      #:msg "division by zero\n")
     (bvudiv v1 v2)]
    [(BPF_MOD)
     (core:bug-on (core:bvzero? v2) #:dbg current-pc-debug
      #:msg "division by zero\n")
     (bvurem v1 v2)]
    [(BPF_OR) (bvor v1 v2)]
    [(BPF_AND) (bvand v1 v2)]
    [(BPF_XOR) (bvxor v1 v2)]
    [(BPF_LSH) (bvshl v1 v2)]
    [(BPF_RSH) (bvlshr v1 v2)]
    [(BPF_ARSH) (bvashr v1 v2)]
    [else (core:bug-on #t #:dbg current-pc-debug
           #:msg (format "no such ALU op: ~e\n" op))]))

(define (evaluate-alu32 op v1 v2)
  (zero-extend (evaluate-alu64 op (extract 31 0 v1) (extract 31 0 v2)) (bitvector 64)))


(define (evaluate-cond op v1 v2)
  (case op
    [(BPF_JEQ)
     (cond
       ; short-circuit bvzero? if v2 is not bv
       [(&& (core:pointer? v1) (if (bv? v2) (core:bvzero? v2) #f)) #f]
       [(&& (bv? v1) (bv? v2)) (bveq v1 v2)]
       [(&& (core:pointer? v1) (core:pointer? v2)) (equal? v1 v2)]
       [else (core:bug-on #t #:dbg current-pc-debug
              #:msg (format "invalid ~e: ~e ~e\n" op v1 v2))])]
    [(BPF_JNE) (! (evaluate-cond 'BPF_JEQ v1 v2))]
    ; maybe too strict for pointer comparisons
    [(BPF_JGT) (bvugt v1 v2)]
    [(BPF_JLT) (bvult v1 v2)]
    [(BPF_JGE) (bvuge v1 v2)]
    [(BPF_JLE) (bvule v1 v2)]
    [(BPF_JSET) (! (core:bvzero? (bvand v1 v2)))]
    [(BPF_JSGT) (bvsgt v1 v2)]
    [(BPF_JSLT) (bvslt v1 v2)]
    [(BPF_JSGE) (bvsge v1 v2)]
    [(BPF_JSLE) (bvsle v1 v2)]
    [else (core:bug-on #t #:dbg current-pc-debug
           #:msg (format "no such comparison op: ~e\n" op))]))


(define (fd->map cpu fd)
  (vector-ref (cpu-fdtable cpu) (bitvector->natural fd)))

(define (evaluate-call-map_lookup_elem cpu)
  (define fd (reg-ref cpu BPF_REG_ARG1))
  (define keyp (reg-ref cpu BPF_REG_ARG2))
  (bpf-map-lookup (fd->map cpu fd) keyp))

(define bpf-calls (vector #f
  evaluate-call-map_lookup_elem))


(define (bpf-size->integer size)
  (case size
    [(BPF_B) 1]
    [(BPF_H) 2]
    [(BPF_W) 4]
    [(BPF_DW) 8]
    [else (core:bug-on #t #:dbg current-pc-debug
           #:msg (format "unknown BPF_SIZE: ~e\n" size))]))

; assume little-endian
(define bitvector->list core:bitvector->list/le)
(define list->bitvector core:list->bitvector/le)

; n must be a constant
(define (load-bytes ptr n)
  (define block (core:pointer-base ptr))
  (define offset (core:pointer-offset ptr))
  (core:bug-on (not (core:bvaligned? offset n)) #:dbg current-pc-debug
   #:msg (format "load-bytes: ~a not ~a-byte aligned\n" ptr n))
  (core:spectre-bug-on (not (core:mblock-inbounds? block offset (core:bvpointer n))) #:dbg current-pc-debug
   #:msg (format "spectre: ~a-byte load @ ~a\n" n ptr))
  (for/list ([i (in-range n)])
    (define path (core:mblock-path block (bvadd offset (bv i 64)) (bv 1 64) #:dbg current-pc-debug))
    (core:mblock-iload block path)))

(define (store-bytes! ptr data)
  (define block (core:pointer-base ptr))
  (define offset (core:pointer-offset ptr))
  (define n (length data))
  (core:bug-on (not (core:bvaligned? offset n)) #:dbg current-pc-debug
           #:msg (format "store-bytes: ~a not ~a-byte aligned\n" ptr n))
  (for ([c (in-list data)] [i (in-range n)])
    (define path (core:mblock-path block (bvadd offset (bv i 64)) (bv 1 64) #:dbg current-pc-debug))
    (core:mblock-istore! block c path)))


(define (cpu-next! cpu [size (bv 1 64)])
  (set-cpu-pc! cpu (bvadd size (cpu-pc cpu))))


(define (sign-imm64 x)
  (sign-extend x (bitvector 64)))

(define (zero-imm64 x)
  (zero-extend x (bitvector 64)))

(define (imm64-dw lo hi)
  (bvor (zero-extend lo (bitvector 64))
        (bvshl (zero-extend hi (bitvector 64)) (bv 32 64))))

(define (endian-size imm)
  (cond
    [(equal? imm (bv 16 32)) 2]
    [(equal? imm (bv 32 32)) 4]
    [(equal? imm (bv 64 32)) 8]
    [else (core:bug-on #t #:dbg current-pc-debug
           #:msg (format "unknown endian size ~e\n" imm))]))


; Interpret an instruction. Returns #t if PC should be bumped after
(define (interpret-instr cpu code dst src off imm)
  (match code
    ; debugging
    [(list 'BPF_TRACE)
     (displayln (cons (bitvector->natural imm) cpu))]

    ; neg instructions (before other ALU operations)
    [(list 'BPF_ALU64 'BPF_NEG)
     (reg-set! cpu dst (bvneg (reg-ref cpu dst)))]

    [(list 'BPF_ALU 'BPF_NEG)
     (reg-set! cpu dst (zero-extend (bvneg (extract 31 0 (reg-ref cpu dst))) (bitvector 64)))]

    ; mov instructions (before other ALU operations)
    [(list 'BPF_ALU64 'BPF_MOV 'BPF_K)
     (reg-set! cpu dst (sign-imm64 imm))]

    [(list 'BPF_ALU 'BPF_MOV 'BPF_K)
     (reg-set! cpu dst (zero-imm64 imm))]

    [(list 'BPF_ALU64 'BPF_MOV 'BPF_X)
     ; NB: intentially leak src for poc/zero-1251-log
     (verbose "r~a = r~a (~a)" dst src (reg-ref cpu src))
     (reg-set! cpu dst (reg-ref cpu src))]

    [(list 'BPF_ALU 'BPF_MOV 'BPF_X)
     (reg-set! cpu dst (zero-extend (extract 31 0 (reg-ref cpu src)) (bitvector 64)))]

    ; binary operation with immediate
    [(list 'BPF_ALU64 op 'BPF_K)
      (reg-set! cpu dst (evaluate-alu64 op (reg-ref cpu dst) (sign-imm64 imm)))]

    [(list 'BPF_ALU op 'BPF_K)
     (reg-set! cpu dst (evaluate-alu32 op (reg-ref cpu dst) imm))]

    ; binary operation two registers
    [(list 'BPF_ALU64 op 'BPF_X)
     (cond
       [(&& (|| (equal? op 'BPF_SUB) (equal? op 'BPF_XOR)) (equal? dst src))
        (reg-set! cpu dst (bv 0 64))]
       [else
        (reg-set! cpu dst (evaluate-alu64 op (reg-ref cpu dst) (reg-ref cpu src)))])]

    [(list 'BPF_ALU op 'BPF_X)
     (cond
       [(&& (|| (equal? op 'BPF_SUB) (equal? op 'BPF_XOR)) (equal? dst src))
        (reg-set! cpu dst (bv 0 64))]
       [else
        (reg-set! cpu dst (evaluate-alu32 op (reg-ref cpu dst) (reg-ref cpu src)))])]

    ; unconditional branching
    [(list 'BPF_JMP 'BPF_JA 'BPF_K)
     (cpu-next! cpu (sign-imm64 off))]

    ; conditional branching
    [(list 'BPF_JMP op 'BPF_K)
     (when (evaluate-cond op (reg-ref cpu dst) (sign-imm64 imm))
       (cpu-next! cpu (sign-imm64 off)))]

    [(list 'BPF_JMP op 'BPF_X)
     (when (evaluate-cond op (reg-ref cpu dst) (reg-ref cpu src))
       (cpu-next! cpu (sign-imm64 off)))]

    ; call
    [(list 'BPF_JMP 'BPF_CALL)
     (let ([f (vector-ref bpf-calls (bitvector->natural imm))])
       (reg-set! cpu BPF_REG_0 (f cpu)))
       ; scratch R1-R5
       (for-each (lambda (r) (reg-havoc! cpu r))
                 (list BPF_REG_1 BPF_REG_2 BPF_REG_3 BPF_REG_4 BPF_REG_5))]

    ; load operations
    [(list 'BPF_LDX size 'BPF_MEM)
     (let ([data (load-bytes (core:pointer-add (reg-ref cpu src) off)
                             (bpf-size->integer size))])
       (reg-set! cpu dst (zero-extend (list->bitvector data) (bitvector 64))))]

    ; store operations
    [(list 'BPF_ST size 'BPF_MEM)
     (store-bytes! (core:pointer-add (reg-ref cpu dst) off)
                   (take (bitvector->list (sign-imm64 imm))
                         (bpf-size->integer size)))]

    [(list 'BPF_STX size 'BPF_MEM)
     (store-bytes! (core:pointer-add (reg-ref cpu dst) off)
                   (take (bitvector->list (reg-ref cpu src))
                         (bpf-size->integer size)))]

    ; xadd operations
    [(list 'BPF_STX 'BPF_W 'BPF_XADD)
     (define ptr (core:pointer-add (reg-ref cpu dst) off))
     (define old (list->bitvector (load-bytes ptr 4)))
     (define new (bvadd old (extract 31 0 (reg-ref cpu src))))
     (store-bytes! ptr (bitvector->list new))]

    [(list 'BPF_STX 'BPF_DW 'BPF_XADD)
     (define ptr (core:pointer-add (reg-ref cpu dst) off))
     (define old (list->bitvector (load-bytes ptr 8)))
     (define new (bvadd old (reg-ref cpu src)))
     (store-bytes! ptr (bitvector->list new))]

    ; endian operations
    [(list 'BPF_ALU 'BPF_END 'BPF_FROM_BE)
      (define old (reg-ref cpu dst))
      (define new
        (cond

          [(equal? imm (bv 16 32))
            (zero-extend
              (concat (extract 7 0 old) (extract 15 8 old))
              (bitvector 64))]

          [(equal? imm (bv 32 32))
            (zero-extend
              (concat (extract 7 0 old) (extract 15 8 old) (extract 23 16 old) (extract 31 24 old))
              (bitvector 64))]

          [(equal? imm (bv 64 32))
            (zero-extend
              (concat (extract 7 0 old) (extract 15 8 old) (extract 23 16 old) (extract 31 24 old)
                      (extract 39 32 old) (extract 47 40 old) (extract 55 48 old) (extract 63 56 old))
              (bitvector 64))]

          [#t (core:bug-on #t)]))
        (reg-set! cpu dst new)]

    [(list 'BPF_ALU 'BPF_END 'BPF_FROM_LE)
     (reg-set! cpu dst (zero-extend (core:list->bitvector/le (take (bitvector->list (reg-ref cpu dst)) (endian-size imm)))
                                    (bitvector 64)))]

    ; default
    [_ (core:bug-on #t #:dbg current-pc-debug
        #:msg (format "no semantics for instruction ~e\n" code))])
  (cpu-next! cpu))


(define (verbose-cpu)
  (verbose "~a" cpu))

(define (verbose fmt . args)
  (define out (bpf-verbose))
  (when out
    (core:bug-on (&& (not (bpf-allow-leak-pointer)) (ormap core:pointer? args))
     #:msg (format "verbose: pointer not allowed: ~e\n" args) #:dbg current-pc-debug)
    (displayln (apply format (cons fmt args)) out)))

(define (interpret-program cpu instructions)

  (for/all ([pc (cpu-pc cpu) #:exhaustive]) (begin
    (set-cpu-pc! cpu pc)

    (cond
      [(hash-has-key? instructions pc)
        (update-seen! cpu instructions pc)
        (match (hash-ref instructions pc)
          ; exit
          [(struct insn ('(BPF_JMP BPF_EXIT) _ _ _ _))
           (extract 31 0 (reg-ref cpu BPF_REG_0))]

          ; ld64
          [(struct insn ('(BPF_LD BPF_DW BPF_IMM) dst src #f lower))
           (core:bug-on (not (hash-has-key? instructions pc)) #:dbg current-pc-debug
            #:msg (format "no instruction after LD64 @ ~e\n" pc))
           (define pc2 (core:bvadd1 pc))
           (update-seen! cpu instructions pc2)
           (match (hash-ref instructions pc2)
             [(struct insn (#f #f #f #f upper))
              (reg-set! cpu dst (imm64-dw lower upper))
              (cpu-next! cpu)
              (cpu-next! cpu)
              (interpret-program cpu instructions)]
             [any (core:bug-on #t #:dbg pc2 #:msg (format "bad instruction after LD64 @ ~e\n" pc2))])]

          [(struct insn (code dst src off imm))
           (interpret-instr cpu code dst src off imm)
           (interpret-program cpu instructions)]

          [any (core:bug-on #t #:dbg current-pc-debug
                #:msg (format "bad instruction format ~e\n" any))])]
      [#t (core:bug-on #t #:dbg current-pc-debug
           #:msg (format "no instruction @ ~e\n" pc))]))))

(define (sanitize-program cpu instructions)
  (define seen (cpu-seen cpu))
  (define nop (BPF_MOV64_REG BPF_REG_0 BPF_REG_0))
  (for/hash ([(pc insn) (in-hash instructions)])
    (define dead (not (member pc seen)))
    ; could invoke the solver; just do a simple constant evaluation here
    (define kill (&& (not (term? dead)) dead))
    (when kill
      (displayln (format "removing dead code: ~a . ~a" (bitvector->natural pc) insn)))
    (values pc (if kill nop insn))))
