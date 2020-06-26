#lang rosette

(require serval/riscv/base
         serval/riscv/interp2
         serval/riscv/decode)

(define-symbolic rs1 rs2 rd (bitvector 5))

(displayln (decode (concat (bv 0 7) rs2 rs1 (bv 0 3) rd (bv #b0110011 7))))