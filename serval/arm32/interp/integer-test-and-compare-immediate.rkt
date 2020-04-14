#lang rosette

(require
  "common.rkt")

(provide
  tst-immediate
  teq-immediate
  cmp-immediate
  cmn-immediate)


(define (decode cpu Rn imm12)
  (define n Rn)
  (define-values (imm32 carry) (a32-expand-imm_c imm12 (cpu-pstate.c cpu)))
  (values n imm32 carry))

(define (interpret-t* proc cpu Rn imm12)
  (define-values (n imm32 carry) (decode cpu Rn imm12))
  (define result (proc (cpu-gpr-ref cpu n) imm32))
  (cpu-pstate.n-set! cpu (bit 31 result))
  (cpu-pstate.z-set! cpu (is-zero-bit result))
  (cpu-pstate.c-set! cpu carry)
  ; PSTATE.V unchanged
)

(define interpret-tst
  (curry interpret-t* bvand))

(define interpret-teq
  (curry interpret-t* bvxor))

(define (interpret-c* proc cpu Rn imm12)
  (define-values (n imm32 _) (decode cpu Rn imm12))
  (define-values (result nzcv) (add-with-carry (cpu-gpr-ref cpu n) (proc imm32) (proc (bv #b0 1))))
  (cpu-pstate.nzcv-set! cpu nzcv))

(define interpret-cmp
  (curry interpret-c* bvnot))

(define interpret-cmn
  (curry interpret-c* identity))


(define-insn (Rn imm12)
  #:encode (lambda (opc) (list (bv #b00110 5) (bv opc 2) (bv #b1 1) Rn (bv #b0000 4) imm12))
  [(#b00) tst-immediate interpret-tst]
  [(#b01) teq-immediate interpret-teq]
  [(#b10) cmp-immediate interpret-cmp]
  [(#b11) cmn-immediate interpret-cmn])
