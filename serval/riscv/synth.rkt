#lang rosette/safe

(require
  "base.rkt"
  "interp.rkt"
  rosette/lib/synthax
  rosette/lib/angelic
)

(provide (all-defined-out))

(define (binary-op)
  (choose*
    'addi
    'subi))

(define (gpr) (choose* 'x0 'x1 'x2 'x3 'x4 'x5 'x6 'x7 'x8 'x9 'x10 'x11 'x12 'x13 'x14 'x15))

(define-synthax ??-binary-op-imm
  ([(_)
    (let
      ([op (binary-op)]
       [dst (gpr)]
       [src1 (gpr)]
       [src2 'x0]
       [imm (?? (bitvector 12))]
       [size 4])
      (instr op dst src1 src2 imm size))]))
