#lang rosette

(require
  (for-syntax
    (only-in racket/syntax format-id))
  "../base.rkt"
  "../decode.rkt")

(provide define-insn define-insn/32 define-insn/64)

; The main macro for defining instructions.

(define-syntax (define-insn stx)
  (syntax-case stx ()
    [(_ (arg ...) #:encode encode [(field ...) op interp] ...)
      #'(define-insn-xlen (arg ...) #:xlen (list 32 64) #:encode encode [(field ...) op interp] ...)]))

(define-syntax (define-insn/32 stx)
  (syntax-case stx ()
    [(_ (arg ...) #:encode encode [(field ...) op interp] ...)
      #'(define-insn-xlen (arg ...) #:xlen (list 32) #:encode encode [(field ...) op interp] ...)]))

(define-syntax (define-insn/64 stx)
  (syntax-case stx ()
    [(_ (arg ...) #:encode encode [(field ...) op interp] ...)
      #'(define-insn-xlen (arg ...) #:xlen (list 64) #:encode encode [(field ...) op interp] ...)]))

(define-syntax (define-insn-xlen stx)
  (syntax-case stx ()
    [(_ (arg ...) #:xlen mode #:encode encode [(field ...) op interp] ...)
     #'(begin
         (struct op (arg ...)
          #:transparent
          #:guard (lambda (arg ... name)
                    (assert (memv (XLEN) mode))
                    (values
                      ; split for type checking
                      (for/all ([arg arg #:exhaustive])
                        (guard arg)
                        arg) ...))
          #:methods gen:instruction
          [(define (instruction-encode insn)
             (define lst
               (match-let ([(op arg ...) insn])
                 ((lambda (arg ...) (encode field ...)) arg ...)))
             (apply concat (map (lambda (x) (if (box? x) (unbox x) x)) lst)))
           (define (instruction-run insn cpu)
             (match-let ([(op arg ...) insn])
               (interp cpu insn arg ...)))]) ...
         (add-decoder mode op
           ((lambda (arg ...) (encode field ...)) (typeof arg) ...))
         ... )]))

; Type checking guards.

(define-syntax-rule (guard v)
  (let ([type (typeof v)])
    (define expr
      (cond
        [(box? type)
         (set! type (unbox type))
         (and (box? v) (type (unbox v)))]
        [else
         (type v)]))
    (assert expr (format "~a: expected type ~a" v type))))

(define-syntax (typeof stx)
  (syntax-case stx ()
    [(_ arg)
     (with-syntax ([type (format-id stx "typeof-~a" (syntax-e #'arg))])
       #'type)]))

(define typeof-aq (bitvector 1))
(define typeof-csr (bitvector 12))
(define typeof-fm (bitvector 4))
(define typeof-funct2 (bitvector 2))
(define typeof-funct3 (bitvector 3))
(define typeof-funct4 (bitvector 4))
(define typeof-funct6 (bitvector 6))
(define typeof-funct7 (bitvector 7))
(define typeof-imm11:0 (bitvector 12))
(define typeof-imm11:5 (bitvector 7))
(define typeof-imm12&10:5 (bitvector 7))
(define typeof-imm20&10:1&11&19:12 (bitvector 20))
(define typeof-imm11&4&9:8&10&6&7&3:1&5 (bitvector 11))
(define typeof-imm31:12 (bitvector 20))
(define typeof-imm4:0 (bitvector 5))
(define typeof-imm4:1&11 (bitvector 5))
(define typeof-imm5 (bitvector 1))
(define typeof-imm7:6&2:1&5 (bitvector 5))
(define typeof-imm8&4:3 (bitvector 3))
(define typeof-opcode (bitvector 7))
(define typeof-pred (bitvector 4))
(define typeof-rd (bitvector 5))
(define typeof-rl (bitvector 1))
(define typeof-rs1 (bitvector 5))
(define typeof-rs2 (bitvector 5))
(define typeof-shamt5 (bitvector 5))
(define typeof-shamt6 (bitvector 6))
(define typeof-succ (bitvector 4))
(define typeof-uimm (bitvector 5))
(define typeof-uimm5 (bitvector 1))
(define typeof-rs1/rd (bitvector 5))

; Compressed instruction register fields.
(define typeof-rd^ (bitvector 3))
(define typeof-rs1^ (bitvector 3))
(define typeof-rs2^ (bitvector 3))
(define typeof-rs1^/rd^ (bitvector 3))

(define typeof-nz-rs1/rd (nonzero 5))
(define typeof-nz-rs2 (nonzero 5))
(define typeof-nz-rd (nonzero 5))
(define typeof-nz-rs1 (nonzero 5))

; c.lui rd can be any reg except x0, x2
(define typeof-c.lui-rd (exclude (list 0 2) 5))
(define typeof-nzimm17 (bitvector 1))
(define typeof-nzimm16:12 (bitvector 5))

(define typeof-uimm5:3 (bitvector 3))
(define typeof-uimm7:6 (bitvector 2))
(define typeof-uimm2&6 (bitvector 2))

(define typeof-nzimm4&6&8:7&5 (bitvector 5))
(define typeof-nzimm9 (bitvector 1))

(define typeof-uimm5:2&7:6 (bitvector 6))
(define typeof-uimm5:3&8:6 (bitvector 6))

(define typeof-nzuimm5:4&9:6&2&3 (nonzero 8))