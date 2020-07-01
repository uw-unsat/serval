#lang rosette

(require "common.rkt")

(provide (all-defined-out))

;;; Common

(define ((interpret-shift-immediate op dec-reg) cpu insn imm5 rd imm4:0)
  (define xlen (cpu-xlen cpu))
  (define imm (zero-extend (concat imm5 imm4:0) (bitvector xlen)))

  (core:bug-on (&& (= xlen 32) (! (bvzero? imm5)))
               #:msg (format "~v: imm5 must be 0 for RV32" insn))
  (core:bug-on (bvzero? imm)
               #:msg (format "~v: shift amount must be non-zero" insn))
  (define a (gpr-ref cpu (dec-reg rd)))
  (gpr-set! cpu (dec-reg rd) (op a imm))
  (cpu-next! cpu insn))

;;; CR-type "Register" instructions

(define ((interpret-cr-2reg-type op) cpu insn rs1/rd rs2)
  (reg-reg-op op cpu insn (decode-gpr rs2) (decode-gpr rs1/rd) (decode-gpr rs1/rd)))

; non-zero rs1/rd, non-zero rs2
(define-insn (nz-rs1/rd nz-rs2)
  #:encode (lambda (funct4 op)
                   (list (bv funct4 4) nz-rs1/rd nz-rs2 (bv op 2)))
  [(#b1000 #b10) c.mv  (interpret-cr-2reg-type (lambda (a b) b))]
  [(#b1001 #b10) c.add (interpret-cr-2reg-type bvadd)])

; non-zero rs1/rd, zero rs2
(define-insn (nz-rs1/rd)
  #:encode (lambda (funct4 op)
                   (list (bv funct4 4) nz-rs1/rd (bv 0 5) (bv op 2)))
  [(#b1000 #b10) c.jr skip/debug]
  [(#b1001 #b10) c.jalr skip/debug])

; zero rs1/rd, zero rs2
(define-insn ()
  #:encode (lambda (funct4 op)
                   (list (bv funct4 4) (bv 0 5) (bv 0 5) (bv op 2)))
  [(#b1001 #b10) c.ebreak skip/debug])

;;; CI-type "Immediate" instructions

; non-zero rd
(define-insn (imm5 nz-rd imm4:0)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) imm5 nz-rd imm4:0 (bv op 2)))
  [(#b000 #b01) c.addi skip/debug]
  [(#b010 #b01) c.li skip/debug]

  [(#b000 #b10) c.slli (interpret-shift-immediate bvshl decode-gpr)])

; zero rd
(define-insn (imm5 imm4:0)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) imm5 (bv 0 5) imm4:0 (bv op 2)))
  [(#b000 #b01) c.nop skip/debug])

;;; CSS-type "Stack-relative Store" instructions

;;; CIW-type "Wide Immediate" Instructions

;;; CL-type "Load" instructions

;;; CS-type "Store" instructions

;;; CA-type "Arithmetic" instructions

(define ((interpret-ca-type op) cpu insn rs1^/rd^ rs2^)
  (reg-reg-op op cpu insn (decode-compressed-gpr rs2^) (decode-compressed-gpr rs1^/rd^) (decode-compressed-gpr rs1^/rd^)))

(define ((interpret-caw-type op) cpu insn rs1^/rd^ rs2^)
  (reg-reg-opw op cpu insn (decode-compressed-gpr rs2^) (decode-compressed-gpr rs1^/rd^) (decode-compressed-gpr rs1^/rd^)))

(define-insn (rs1^/rd^ rs2^)
  #:encode (lambda (funct6 funct2 op)
                   (list (bv funct6 6) rs1^/rd^ (bv funct2 2) rs2^ (bv op 2)))
  [(#b100011 #b00 #b01) c.sub  (interpret-ca-type bvsub)]
  [(#b100011 #b01 #b01) c.xor  (interpret-ca-type bvxor)]
  [(#b100011 #b10 #b01) c.or   (interpret-ca-type bvor)]
  [(#b100011 #b11 #b01) c.and  (interpret-ca-type bvand)]

  [(#b100111 #b00 #b01) c.subw (interpret-caw-type bvsub)]
  [(#b100111 #b01 #b01) c.addw (interpret-caw-type bvadd)])

;;; CB-type "Branch" instructions

(define ((interpret-c-branch op) cpu insn imm8&4:3 rs1^ imm7:6&2:1&5)
  (define xlen (cpu-xlen cpu))

  (define off
    (concat (extract 2 2 imm8&4:3)
            (extract 4 3 imm7:6&2:1&5)
            (extract 0 0 imm7:6&2:1&5)
            (extract 1 0 imm8&4:3)
            (extract 2 1 imm7:6&2:1&5)
            (bv 0 1)))

  (define a (gpr-ref cpu (decode-compressed-gpr rs1^)))
  (define branch (op a))

  (if branch
    (set-cpu-pc! cpu (bvadd (cpu-pc cpu) (sign-extend off (bitvector xlen))))
    (cpu-next! cpu insn)))

; Regular branch instructions
(define-insn (imm8&4:3 rs1^ imm7:6&2:1&5)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) imm8&4:3 rs1^ imm7:6&2:1&5 (bv op 2)))
  [(#b110 #b01) c.beqz (interpret-c-branch bvzero?)]
  [(#b111 #b01) c.bnez (interpret-c-branch (compose ! bvzero?))])

(define ((interpret-cb-imm-type op) cpu insn imm5 rs1^/rd^ imm4:0)
  (reg-imm-op op cpu insn (concat imm5 imm4:0) (decode-compressed-gpr rs1^/rd^) (decode-compressed-gpr rs1^/rd^)))

(define ((interpret-cb-shift-type op) cpu insn imm5 rs1^/rd^ imm4:0)
  (define xlen (cpu-xlen cpu))
  (define imm (zero-extend (concat imm5 imm4:0) (bitvector xlen)))

  (core:bug-on (&& (= xlen 32) (! (bvzero? imm5)))
               #:msg (format "~v: imm5 must be 0 for RV32" insn))
  (core:bug-on (bvzero? imm)
               #:msg (format "~v: shift amount must be non-zero" insn))
  (define a (gpr-ref cpu (decode-compressed-gpr rs1^/rd^)))
  (gpr-set! cpu (decode-compressed-gpr rs1^/rd^) (op a imm))
  (cpu-next! cpu insn))

; "c.andi" is a CB-type as well
(define-insn (imm5 rs1^/rd^ imm4:0)
  #:encode (lambda (funct3 funct2 op)
                   (list (bv funct3 3) imm5 (bv funct2 2) rs1^/rd^ imm4:0 (bv op 2)))
  [(#b100 #b00 #b01) c.srli (interpret-shift-immediate bvlshr decode-compressed-gpr)]
  [(#b100 #b01 #b01) c.srai (interpret-shift-immediate bvashr decode-compressed-gpr)]
  [(#b100 #b10 #b01) c.andi (interpret-cb-imm-type bvand)])

;;; CJ-type "Jump" instructions

(define-insn (imm11&4&9:8&10&6&7&3:1&5)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) imm11&4&9:8&10&6&7&3:1&5 (bv op 2)))
  ;;; [(#b001 #b01) c.jal skip/debug] NB: rv32-only
  [(#b101 #b01) c.j   skip/debug])

; All zeroes is a special compressed illegal instruction.
(define-insn ()
  #:encode (lambda () (list (bv 0 16)))
  [() c.unimp notimplemented])