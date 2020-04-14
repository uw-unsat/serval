#lang rosette

(require
  "common.rkt")

(provide
  orr-register orrs-register
  mov-register movs-register
  lsl-immediate
  lsr-immediate
  asr-immediate
  ror-immediate)


(define (decode S Rn Rd imm5 stype Rm)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define setflags (bveq S (bv #b1 1)))
  (define-values (shift_t shift_n) (decode-imm-shift stype imm5))
  (values d n m setflags shift_t shift_n))

(define (interpret proc cpu S Rn Rd imm5 stype Rm)
  (define-values (d n m setflags shift_t shift_n) (decode S Rn Rd imm5 stype Rm))
  (define-values (shifted carry) (shift_c (cpu-gpr-ref cpu m) shift_t shift_n (cpu-pstate.c cpu)))
  (define result (proc cpu n shifted))
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
       ; PSTATE.V
     )]))

(define interpret-orr/orrs
  (curry interpret
         (lambda (cpu n shifted)
           (bvor (cpu-gpr-ref cpu n) shifted))))

(define interpret-mov/movs
  (curry interpret
         (lambda (cpu n shifted)
           shifted)))


(define-insn (S Rn Rd imm5 stype Rm)
  #:encode (lambda (opc) (list (bv #b00011 5) (bv opc 2) S Rn Rd imm5 stype (bv #b0 1) Rm))
  [(#b00) orr/orrs-register interpret-orr/orrs]
  [(#b01) mov/movs-register interpret-mov/movs])

(define orr-register
  (curry orr/orrs-register (bv #b0 1)))

(define orrs-register
  (curry orr/orrs-register (bv #b1 1)))

(define mov-register
  (curry mov/movs-register (bv #b0 1) (integer->gpr 0)))

(define movs-register
  (curry mov/movs-register (bv #b1 1) (integer->gpr 0)))

; aliases

(define (lsl-immediate Rd imm5 Rm)
  (mov-register Rd imm5 (bv #b00 2) Rm))

(define (lsr-immediate Rd imm5 Rm)
  (mov-register Rd imm5 (bv #b01 2) Rm))

(define (asr-immediate Rd imm5 Rm)
  (mov-register Rd imm5 (bv #b10 2) Rm))

(define (ror-immediate Rd imm5 Rm)
  (mov-register Rd imm5 (bv #b11 2) Rm))
