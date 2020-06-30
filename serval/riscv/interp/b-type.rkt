#lang rosette

(require "common.rkt")

(provide (all-defined-out))

(define ((interpret-b-type op) cpu insn imm12&10:5 rs2 rs1 imm4:1&11)
  (define xlen (cpu-xlen cpu))
  (define off
    (concat (extract 6 6 imm12&10:5)
            (extract 0 0 imm4:1&11)
            (extract 5 0 imm12&10:5)
            (extract 4 1 imm4:1&11)
            (bv 0 1)))

  (define a (gpr-ref cpu (decode-gpr rs1)))
  (define b (gpr-ref cpu (decode-gpr rs2)))

  (define branch (op a b))

  (if branch
    (set-cpu-pc! cpu (bvadd (cpu-pc cpu) (sign-extend off (bitvector xlen))))
    (cpu-next! cpu insn)))

(define-insn (imm12&10:5 rs2 rs1 imm4:1&11)
  #:encode (lambda (funct3 opcode)
                   (list imm12&10:5 rs2 rs1 (bv funct3 3) imm4:1&11 (bv opcode 7)))
  [(#b000 #b1100011) beq  (interpret-b-type bveq)]
  [(#b001 #b1100011) bne  (interpret-b-type (lambda (a b) (! (bveq a b))))]
  [(#b100 #b1100011) blt  (interpret-b-type bvslt)]
  [(#b101 #b1100011) bge  (interpret-b-type bvsge)]
  [(#b110 #b1100011) bltu (interpret-b-type bvult)]
  [(#b111 #b1100011) bgeu (interpret-b-type bvuge)])
