#lang rosette

(require
  "common.rkt")

(provide
  movw movt)


(define (decode imm4 Rd imm12)
  (define d Rd)
  (define imm16 (concat imm4 imm12))
  (when (r15? d)
    (unpredictable))
  (values d imm16))

(define (interpret-movw cpu imm4 Rd imm12)
  (define-values (d imm16) (decode imm4 Rd imm12))
  (cpu-gpr-set! cpu d (zero-extend imm16 (bitvector 32))))

(define (interpret-movt cpu imm4 Rd imm12)
  (define-values (d imm16) (decode imm4 Rd imm12))
  ; R[d]<15:0> unchanged
  (define lower (extract 15 0 (cpu-gpr-ref cpu d)))
  (cpu-gpr-set! cpu d (concat imm16 lower)))


(define-insn (imm4 Rd imm12)
  #:encode (lambda (H) (list (bv #b00110 5) (bv H 1) (bv #b00 2) imm4 Rd imm12))
  [(#b0) movw interpret-movw]
  [(#b1) movt interpret-movt])
