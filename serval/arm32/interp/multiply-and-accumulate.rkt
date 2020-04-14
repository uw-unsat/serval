#lang rosette

(require
  "common.rkt")

(provide
  mul muls
  mls
  umull umulls)


(define (decode-mul/muls S Rd Rm Rn)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define setflags (bveq S (bv #b1 1)))
  (when (|| (r15? d) (r15? n) (r15? m))
    (unpredictable))
  (values d n m setflags))

(define (interpret-mul/muls cpu S Rd dummy Rm Rn)
  (define-values (d n m setflags) (decode-mul/muls S Rd Rm Rn))
  (define operand1 (cpu-gpr-ref cpu n))
  (define operand2 (cpu-gpr-ref cpu m))
  (define result ((core:bvmul-proc) operand1 operand2))
  (cpu-gpr-set! cpu d result)
  (when setflags
    (cpu-pstate.n-set! cpu (bit 31 result))
    (cpu-pstate.z-set! cpu (is-zero-bit result))
    ; PSTATE.C, PSTATE.V unchanged
  ))


(define (decode-mls Rd Ra Rm Rn)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define a Ra)
  (when (|| (r15? d) (r15? n) (r15? m) (r15? a))
    (unpredictable))
  (values d n m a))

(define (interpret-mls cpu S Rd Ra Rm Rn)
  (define-values (d n m a) (decode-mls Rd Ra Rm Rn))
  (define operand1 (cpu-gpr-ref cpu n))
  (define operand2 (cpu-gpr-ref cpu m))
  (define addend (cpu-gpr-ref cpu a))
  (define result (bvsub addend ((core:bvmul-proc) operand1 operand2)))
  (cpu-gpr-set! cpu d result))


(define (decode-umull/umulls S RdHi RdLo Rm Rn)
  (define dLo RdLo)
  (define dHi RdHi)
  (define n Rn)
  (define m Rm)
  (define setflags (bveq S (bv #b1 1)))
  (when (|| (r15? dLo) (r15? dHi) (r15? n) (r15? m))
    (unpredictable))
  (when (equal? dLo dHi)
    (unpredictable))
  (values dLo dHi n m setflags))

(define (interpret-umull/umulls cpu S RdHi RdLo Rm Rn)
  (define-values (dLo dHi n m setflags) (decode-umull/umulls S RdHi RdLo Rm Rn))
  (define operand1 (cpu-gpr-ref cpu n))
  (define operand2 (cpu-gpr-ref cpu m))
  (define lower ((core:bvmul-proc) operand1 operand2))
  (define upper ((core:bvmulhu-proc) operand1 operand2))
  (define result (concat upper lower))
  (cpu-gpr-set! cpu dHi upper)
  (cpu-gpr-set! cpu dLo lower)
  (when setflags
    (cpu-pstate.n-set! cpu (bit 63 result))
    (cpu-pstate.z-set! cpu (is-zero-bit result))
    ; PSTATE.C, PSTATE.V unchanged
  ))


(define-insn (S RdHi RdLo Rm Rn)
  #:encode (lambda (opc) (list (bv #b0000 4) (bv opc 3) S RdHi RdLo Rm (bv #b1001 4) Rn))
  [(#b000) mul/muls     interpret-mul/muls]
  [(#b011) @mls         interpret-mls]
  [(#b100) umull/umulls interpret-umull/umulls])

(define (mul Rd Rm Rn)
  (curry mul/muls (bv #b0 1) Rd (integer->gpr 0) Rm Rn))

(define (muls Rd Rm Rn)
  (curry mul/muls (bv #b1 1) Rd (integer->gpr 0) Rm Rn))

(define mls
  (curry @mls (bv #b0 1)))

(define umull
  (curry umull/umulls (bv #b0 1)))

(define umulls
  (curry umull/umulls (bv #b1 1)))
