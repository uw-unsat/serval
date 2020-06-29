#lang rosette

(require "common.rkt")

(provide (all-defined-out))

(define (interpret-jal cpu insn imm20&10:1&11&19:12 rd)
  (define off
    (concat (extract 19 19 imm20&10:1&11&19:12)
            (extract 7 0 imm20&10:1&11&19:12)
            (extract 8 8 imm20&10:1&11&19:12)
            (extract 18 9 imm20&10:1&11&19:12)
            (bv 0 1)))

  (define next (bvadd (cpu-pc cpu) (bv 4 (cpu-xlen cpu))))
  (gpr-set! cpu (decode-gpr rd) next)

  (set-cpu-pc! cpu (bvadd (cpu-pc cpu) (sign-extend off (bitvector (cpu-xlen cpu))))))

(define-insn (imm20&10:1&11&19:12 rd)
  #:encode (lambda (opcode)
                   (list imm20&10:1&11&19:12 rd (bv opcode 7)))
  [(#b1101111) jal interpret-jal])