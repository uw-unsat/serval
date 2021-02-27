#lang rosette

(require "common.rkt")

(provide (all-defined-out))

(define ((interpret-store-insn size) cpu insn imm11:5 rs2 rs1 imm4:0)
  (define mm (cpu-memmgr cpu))
  (define xlen (cpu-xlen cpu))

  (define addr (bvadd (gpr-ref cpu (decode-gpr rs1))
                      (sign-extend (concat imm11:5 imm4:0) (bitvector xlen))))
  (define value (trunc (* 8 size) (gpr-ref cpu (decode-gpr rs2))))

  (core:memmgr-store! mm addr (bv 0 xlen) value (bv size xlen))
  (cpu-next! cpu insn))

(define-insn (imm11:5 rs2 rs1 imm4:0)
  #:encode (lambda (funct3 opcode)
                   (list imm11:5 rs2 rs1 (bv funct3 3) imm4:0 (bv opcode 7)))

  [(#b000 #b0100011) sb (interpret-store-insn 1)]
  [(#b001 #b0100011) sh (interpret-store-insn 2)]
  [(#b010 #b0100011) sw (interpret-store-insn 4)]
  [(#b011 #b0100011) sd (interpret-store-insn 8)])
