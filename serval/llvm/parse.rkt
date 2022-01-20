#lang racket/base

(require racket/generator
         racket/list
         racket/match
         racket/sequence
         racket/string
         (only-in rosette/base/base bitvector bitvector->natural bv)
         (only-in serval/lib/core marray mcell mfield mstruct)
         "base.rkt"
         "capi/core.rkt"
         "capi/irreader.rkt"
         "capi/target.rkt")

(provide bytes->module)

; parser

(define (unnamed? v)
  (zero? (bytes-length (LLVMGetValueName2 v))))

(define (set-name! v cnt)
  (LLVMSetValueName2 v (string->bytes/utf-8 (number->string cnt))))

(define (bytes->module bstr)
  (define m (LLVMParseIRInContext bstr))
  (define td (LLVMGetModuleDataLayout m))

  (define globals
    (let ([counter 0])
      (for/list ([gv (in-globals m)])
        ; assign names to unnamed global variables (e.g., from ubsan)
        (when (unnamed? gv)
          (set-name! gv counter)
          (set! counter (add1 counter)))
        ; just names for global variables
        (value-ref gv td))))

  (define functions
    (for/list ([f (in-functions m)]
               #:when (let ([s (value->name/string f)]) (not (or (builtin? s) (ubsan? s)))))

      ; assign names to local variables
      (let ([counter 0])
        (for ([arg (in-arguments f)] #:when (unnamed? arg))
          (set-name! arg counter)
          (set! counter (add1 counter)))
        (for ([bb (in-basic-blocks f)])
          (define bbv (LLVMBasicBlockAsValue bb))
          (when (unnamed? bbv)
            (set-name! bbv counter)
            (set! counter (add1 counter)))
          (for ([insn (in-instructions bb)] #:when (and (typeof-ref insn) (unnamed? insn)))
            (set-name! insn counter)
            (set! counter (add1 counter)))))

      (define fname (value-ref f td))
      (define args
        (for/list ([arg (in-arguments f)])
          (argument (value-ref arg td) (typeof-ref arg))))
      (define blocks
        (for/list ([bb (in-basic-blocks f)])
          (define bbname (value-ref bb td))
          (define insns
            (for/list ([insn (in-instructions bb)])
              (define type (typeof-ref insn))
              (define iname (if type (value-ref insn td) #f))
              (define opcode (LLVMGetInstructionOpcode insn))
              (define operands
                (case opcode
                  [(phi) (for/list ([i (in-range (LLVMCountIncoming insn))])
                           (cons (value-ref (LLVMGetIncomingValue insn i) td)
                                 (value-ref (LLVMGetIncomingBlock insn i) td)))]
                  [else (for/list ([v (in-operands insn)])
                          (value-ref v td))]))
              (make-instruction td insn iname type opcode operands)))
          (basic-block bbname #f insns)))
      (function fname #f args blocks)))

  (module globals functions
    ))

(define (make-instruction td insn name type opcode operands)
  (define attributes null)
  (case opcode
    [(alloca)
     (set! operands (list (type->block (LLVMGetAllocatedType insn) td)))
     (set! attributes `((align . ,(LLVMGetAlignment insn))))]

    ; swap br successors
    [(br) (match operands
            [(list condition false-succ true-succ)
             (set! operands (list condition true-succ false-succ))]
            [_ #f])]

    ; handle call-related lifting
    [(call) (let-values ([(args target) (split-at-right operands 1)])
              (set! target (car target))
              (cond
                ; lift asm
                [(asm? target)
                 (set! opcode 'asm)
                 (set! operands (cons target args))]
                [else
                 (define fname-raw (if (symbol? target) (symbol->string target) #f))
                 (define fname
                   (if (and fname-raw (string-prefix? fname-raw "@")) (substring fname-raw 1) #f))
                 (cond
                   ; lift ubsan handling
                   [(and fname (ubsan? fname))
                    ; remove @
                    (set! opcode (string->symbol fname))
                    ; lift the first arg
                    (set! operands (cons (lift (LLVMGetOperand insn 0) td) (cdr args)))]
                   ; move call target up
                   [else
                    ; remove @ for builtin functions
                    (when (and fname (builtin? fname))
                      (set! target (string->symbol fname)))
                    (set! operands (cons target args))])]))]

    [(extractvalue insertvalue) (set! operands (append operands (LLVMGetIndices insn)))]

    ; emit byte offsets for gep
    [(getelementptr)
     (let ([type (LLVMTypeOf (LLVMGetOperand insn 0))])
       (define indices
         (for/list ([idx (cdr operands)])
           (case (LLVMGetTypeKind type)
             [(struct) (let* ([elem-idx (bitvector->natural idx)]
                              [offset (LLVMOffsetOfElement td type elem-idx)])
                         (set! type (LLVMStructGetTypeAtIndex type elem-idx))
                         (struct-offset offset))]
             [(pointer array vector)
              (set! type (LLVMGetElementType type))
              (array-offset idx (LLVMABISizeOfType td type))]
             [else (raise 'llvm "unknown GEP type: ~a" (LLVMPrintTypeToString type))])))
       (set! operands (cons (car operands) indices)))]

    ; extract icmp predicate
    [(icmp) (set! opcode (list opcode (LLVMGetICmpPredicate insn)))]

    ; add result type to casts
    [(sext zext trunc ptrtoint) (set! operands (append operands (list type)))]

    ; (switch v #:default default [(val bb) ...])
    [(switch) (match operands
                [(list v default cases ...)
                 (set! operands (append (list v default) (split-to-pairs cases)))])]

    ; append type and align
    [(load)
     (set! operands (append operands (list type)))
     (set! attributes `((align . ,(LLVMGetAlignment insn))))]
    [(store)
     (set! operands (append operands (list (typeof-ref (LLVMGetOperand insn 0)))))
     (set! attributes `((align . ,(LLVMGetAlignment insn))))])

  (instruction name type opcode operands attributes))

(define (lift v td)
  (define kind (LLVMGetValueKind v))
  (cond
    ; strip bitcast
    [(and (equal? kind 'constant-expr) (equal? (LLVMGetConstOpcode v) 'bitcast))
     (lift (LLVMGetOperand (value-> v _LLVMConstantExprRef) 0) td)]
    ; lift global initializer
    [(equal? kind 'global-variable) (let ([v (value-> v _LLVMGlobalVariableRef)])
                                      (when (LLVMGetInitializer v)
                                        (lift (LLVMGetInitializer v) td)))]
    ; lift integer
    [(equal? kind 'constant-int) (LLVMConstIntGetSExtValue (value-> v _LLVMConstantIntRef))]
    ; lift string
    [(and (equal? kind 'constant-data-array) (LLVMIsConstantString v)) (LLVMGetAsString v)]
    ; lift struct
    [(equal? kind 'constant-struct) (for/list ([op (in-operands v)])
                                      (lift op td))]
    ; default
    [else (value-ref v td)]))

(define builtins
  (pregexp (string-join (list "llvm\\.bswap\\.i(16|32|64)"
                              "llvm\\.lifetime\\.(start|end)\\.p0i8"
                              "llvm\\.memset\\.p0i8\\.i64"
                              "llvm\\.[su](add|sub|mul)\\.with\\.overflow\\.i(16|32|64)"
                              "llvm\\.trap"
                              "memset"
                              "memzero_explicit")
                        ")|("
                        #:before-first "^("
                        #:after-last ")$")))

(define (builtin? s)
  (regexp-match builtins s))

(define (ubsan? s)
  (string-prefix? s "__ubsan_handle_"))

(define (type->block type td)
  (define kind (LLVMGetTypeKind type))
  (case kind
    [(array) (marray (LLVMGetArrayLength type) (type->block (LLVMGetElementType type) td))]
    [(struct) (mstruct (LLVMABISizeOfType td type)
                       (for/list ([i (in-range (LLVMCountStructElementTypes type))])
                         (define subtype (LLVMStructGetTypeAtIndex type i))
                         (mfield i
                                 (LLVMOffsetOfElement td type i)
                                 (type->block (LLVMStructGetTypeAtIndex type i) td))))]
    [(integer pointer) (mcell (LLVMABISizeOfType td type))]
    [else (raise-user-error 'llvm "unknown type for block: ~a" (LLVMPrintTypeToString type))]))

; utility functions

(define (split-to-pairs lst)
  (match lst
    [(list x y rest ...) (cons (cons x y) (split-to-pairs rest))]
    [_ null]))

(define (value-ref v td)
  ; convert basic block
  (when (LLVMBasicBlockRef? v)
    (set! v (LLVMBasicBlockAsValue v)))

  (define s (value->name/string v))
  (define named? (non-empty-string? s))
  (case (LLVMGetValueKind v)
    ; inline asm
    ; parse the asm string as the c api doesn't expose access to inline asm
    [(inline-asm) (match (value->string v)
                    [(pregexp #px"asm [a-z ]*\"([^\"]*)\", \"([^\"]*)\"$"
                              (list _ template constraint))
                     (asm template constraint)])]

    ; constant int
    [(constant-int) (bv (LLVMConstIntGetSExtValue (value-> v _LLVMConstantIntRef)) (typeof-ref v))]

    ; null
    [(constant-pointer-null) (nullptr)]

    ; undef
    [(undef-value) (undef (typeof-ref v))]

    ; constant expressions
    [(constant-expr) (let* ([v (value-> v _LLVMConstantExprRef)]
                            [opcode (LLVMGetConstOpcode v)]
                            [operands (for/list ([op (in-operands v)])
                                        (value-ref op td))])
                       (make-instruction td v #f #f opcode operands))]

    ; global
    [(function global-variable)
     (when (not named?)
       (raise-user-error 'llvm "no name for global variable: ~a" (value->string v)))
     (string->symbol (string-append "@" s))]

    ; local
    [(argument basic-block instruction)
     (when (not named?)
       (raise-user-error 'llvm "no name for local variable: ~a" (value->string v)))
     (string->symbol (string-append "%" s))]

    ; debugging
    [(metadata metadata-as-value) (nullptr)]

    [else (raise-user-error 'llvm "unknown value: ~a" (value->string v))]))

(define (typeof-ref v)
  (typeof-type (LLVMTypeOf v)))

(define (typeof-type type)
  (define kind (LLVMGetTypeKind type))
  (case kind
    [(void) #f]
    [(integer) (bitvector (LLVMGetIntTypeWidth type))]
    [(struct) (for/list ([i (in-range (LLVMCountStructElementTypes type))])
                (typeof-type (LLVMStructGetTypeAtIndex type i)))]
    [else kind]))

(define value->string LLVMPrintValueToString)

(define (value->name/string v)
  (bytes->string/utf-8 (LLVMGetValueName2 v)))

(define (in-functions m)
  (in-generator (let loop ([v (LLVMGetFirstFunction m)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextFunction v))))))

(define (in-globals m)
  (in-generator (let loop ([v (LLVMGetFirstGlobal m)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextGlobal v))))))

(define (in-arguments f)
  (in-generator (let loop ([v (LLVMGetFirstParam f)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextParam v))))))

(define (in-basic-blocks f)
  (in-generator (let loop ([v (LLVMGetFirstBasicBlock f)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextBasicBlock v))))))

(define (in-instructions bb)
  (in-generator (let loop ([v (LLVMGetFirstInstruction bb)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextInstruction v))))))

(define (in-operands u)
  (set! u (value-> u _LLVMUserRef))
  (define n (LLVMGetNumOperands u))
  (sequence-map (lambda (i) (LLVMGetOperand u i)) (in-range n)))

(define (insn-opcode i)
  (LLVMGetInstructionOpcode i))
