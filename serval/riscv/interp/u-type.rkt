#lang rosette

(require "common.rkt")

(provide (all-defined-out))

(define (interpret-lui cpu insn imm31:12 rd)
  (gpr-set! cpu (decode-gpr rd)
                (sign-extend (concat imm31:12 (bv 0 12))
                             (bitvector (cpu-xlen cpu))))
  (cpu-next! cpu insn))

(define (interpret-auipc cpu insn imm31:12 rd)
  (gpr-set! cpu (decode-gpr rd)
                (bvadd
                  (sign-extend (concat imm31:12 (bv 0 12)) (bitvector (cpu-xlen cpu)))
                  (cpu-pc cpu)))
  (cpu-next! cpu insn))

(define-insn (imm31:12 rd)
  #:encode (lambda (opcode)
                   (list imm31:12 rd (bv opcode 7)))
  [(#b0110111) lui interpret-lui]
  [(#b0010111) auipc interpret-auipc]
)