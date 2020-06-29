#lang rosette

(require "common.rkt")

(provide (all-defined-out))

; CA-type "Arithmetic" instructions

(define ((interpret-cr-type op) cpu insn rs1^/rd^ rs2^)
  (reg-reg-op op cpu insn (decode-compressed-gpr rs2^) (decode-compressed-gpr rs1^/rd^) (decode-compressed-gpr rs1^/rd^)))

(define ((interpret-crw-type op) cpu insn rs1^/rd^ rs2^)
  (reg-reg-opw op cpu insn (decode-compressed-gpr rs2^) (decode-compressed-gpr rs1^/rd^) (decode-compressed-gpr rs1^/rd^)))

(define-insn (rs1^/rd^ rs2^)
  #:encode (lambda (funct6 funct2 op)
                   (list (bv funct6 6) rs1^/rd^ (bv funct2 2) rs2^ (bv op 2)))
  [(#b100011 #b00 #b01) c.sub (interpret-cr-type bvsub)]
  [(#b100011 #b01 #b01) c.xor (interpret-cr-type bvxor)]
  [(#b100011 #b10 #b01) c.or (interpret-cr-type bvor)]
  [(#b100011 #b11 #b01) c.and (interpret-cr-type bvand)]

  [(#b100111 #b00 #b01) c.subw (interpret-crw-type bvsub)]
  [(#b100111 #b01 #b01) c.addw (interpret-crw-type bvadd)])

; All zeroes is a special compressed illegal instruction.
(define-insn ()
  #:encode (lambda () (list (bv 0 16)))
  [() c.unimp notimplemented])