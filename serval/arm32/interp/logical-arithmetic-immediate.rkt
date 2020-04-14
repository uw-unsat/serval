#lang rosette

(require
  "common.rkt")

(provide
  orr-immediate orrs-immediate
  mov-immediate movs-immediate)


(define (decode cpu S Rn Rd imm12)
  (define d Rd)
  (define n Rn)
  (define setflags (bveq S (bv #b1 1)))
  (define-values (imm32 carry) (a32-expand-imm_c imm12 (cpu-pstate.c cpu)))
  (values d n setflags imm32 carry))

(define (interpret proc cpu S Rn Rd imm12)
  (define-values (d n setflags imm32 carry) (decode cpu S Rn Rd imm12))
  (define result (proc cpu n imm32))
  (cond
    [(r15? d)
     (if setflags
         (alu-exception-return cpu result)
         (alu-write-pc cpu result))]
    [else
     (cpu-gpr-set! cpu d result)
     (when setflags
       (cpu-pstate.n-set! cpu (bit 31 result))
       (cpu-pstate.z-set! cpu (is-zero-bit result))
       (cpu-pstate.c-set! cpu carry)
       ; PSTATE.V unchanged
     )]))

(define interpret-orr/orrs
  (curry interpret
         (lambda (cpu n imm32)
           (bvor (cpu-gpr-ref cpu n) imm32))))

(define interpret-mov/movs
  (curry interpret
         (lambda (cpu n imm32)
           imm32)))


(define-insn (S Rn Rd imm12)
  #:encode (lambda (opc) (list (bv #b00111 5) (bv opc 2) S Rn Rd imm12))
  [(#b00) orr/orrs-immediate interpret-orr/orrs]
  [(#b01) mov/movs-immediate interpret-mov/movs])

(define orr-immediate
  (curry orr/orrs-immediate (bv #b0 1)))

(define orrs-immediate
  (curry orr/orrs-immediate (bv #b1 1)))

(define mov-immediate
  (curry mov/movs-immediate (bv #b0 1) (integer->gpr 0)))

(define movs-immediate
  (curry mov/movs-immediate (bv #b1 1) (integer->gpr 0)))
