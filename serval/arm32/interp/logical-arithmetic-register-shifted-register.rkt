#lang rosette

(require
  "common.rkt")

(provide
  orr-register-shifted-register orrs-register-shifted-register
  mov-register-shifted-register movs-register-shifted-register)


(define (decode S Rn Rd Rs stype Rm)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define s Rs)
  (define setflags (bveq S (bv #b1 1)))
  (define shift_t (decode-reg-shift stype))
  (when (|| (r15? d) (r15? n) (r15? m) (r15? s))
    (unpredictable))
  (values d n m s setflags shift_t))

(define (interpret proc cpu S Rn Rd Rs stype Rm)
  (define-values (d n m s setflags shift_t) (decode S Rn Rd Rs stype Rm))
  (define shift_n (zero-extend (extract 7 0 (cpu-gpr-ref cpu s)) (bitvector 32)))
  (define-values (shifted carry) (shift_c (cpu-gpr-ref cpu m) shift_t shift_n (cpu-pstate.c cpu)))
  (define result (proc cpu n shifted))
  (cpu-gpr-set! cpu d result)
  (when setflags
    (cpu-pstate.n-set! cpu (bit 31 result))
    (cpu-pstate.z-set! cpu (is-zero-bit result))
    (cpu-pstate.c-set! cpu carry)
    ; PSTATE.V unchanged
  ))

(define interpret-orr/orrs
  (curry interpret
         (lambda (cpu n shifted)
           (bvor (cpu-gpr-ref cpu n) shifted))))

(define interpret-mov/movs
  (curry interpret
         (lambda (cpu n shifted)
           shifted)))


(define-insn (S Rn Rd Rs stype Rm)
  #:encode (lambda (opc) (list (bv #b00011 5) (bv opc 2) S Rn Rd Rs (bv #b0 1) stype (bv #b1 1) Rm))
  [(#b00) orr/orrs-register-shifted-register interpret-orr/orrs]
  [(#b01) mov/movs-register-shifted-register interpret-mov/movs])

(define orr-register-shifted-register
  (curry orr/orrs-register-shifted-register (bv #b0 1)))

(define orrs-register-shifted-register
  (curry orr/orrs-register-shifted-register (bv #b1 1)))

(define mov-register-shifted-register
  (curry mov/movs-register-shifted-register (bv #b0 1) (integer->gpr 0)))

(define movs-register-shifted-register
  (curry mov/movs-register-shifted-register (bv #b1 1) (integer->gpr 0)))
