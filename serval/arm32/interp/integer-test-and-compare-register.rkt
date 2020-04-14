#lang rosette

(require
  "common.rkt")

(provide
  tst-register
  teq-register
  cmp-register
  cmn-register)


(define (decode Rn imm5 stype Rm)
  (define n Rn)
  (define m Rm)
  (define-values (shift_t shift_n) (decode-imm-shift stype imm5))
  (values n m shift_t shift_n))

(define (interpret-t* proc cpu Rn imm5 stype Rm)
  (define-values (n m shift_t shift_n) (decode Rn imm5 stype Rm))
  (define-values (shifted carry) (shift_c (cpu-gpr-ref cpu m) shift_t shift_n (cpu-pstate.c cpu)))
  (define result (proc (cpu-gpr-ref cpu n) shifted))
  (cpu-pstate.n-set! cpu (bit 31 result))
  (cpu-pstate.z-set! cpu (is-zero-bit result))
  (cpu-pstate.c-set! cpu carry)
  ; PSTATE.V unchanged
)

(define interpret-tst
  (curry interpret-t* bvand))

(define interpret-teq
  (curry interpret-t* bvxor))

(define (interpret-c* proc cpu Rn imm5 stype Rm)
  (define-values (n m shift_t shift_n) (decode Rn imm5 stype Rm))
  (define shifted (shift (cpu-gpr-ref cpu m) shift_t shift_n (cpu-pstate.c cpu)))
  (define-values (result nzcv) (add-with-carry (cpu-gpr-ref cpu n) (proc shifted) (proc (bv #b0 1))))
  (cpu-pstate.nzcv-set! cpu nzcv))

(define interpret-cmp
  (curry interpret-c* bvnot))

(define interpret-cmn
  (curry interpret-c* identity))


(define-insn (Rn imm5 stype Rm)
  #:encode (lambda (opc) (list (bv #b00010 5) (bv opc 2) (bv #b1 1) Rn (bv #b0000 4) imm5 stype (bv #b0 1) Rm))
  [(#b00) tst-register interpret-tst]
  [(#b01) teq-register interpret-teq]
  [(#b10) cmp-register interpret-cmp]
  [(#b11) cmn-register interpret-cmn])
