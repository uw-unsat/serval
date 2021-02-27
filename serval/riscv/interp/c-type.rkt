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

(define ((interpret-cr-jmp link) cpu insn nz-rs1/rd)
  (define rs1 (decode-gpr nz-rs1/rd))

  (when link
    (define next (bvadd (cpu-pc cpu) (bv 2 (cpu-xlen cpu))))
    (gpr-set! cpu 'ra next))

  (define target
    (for/all ([src (gpr-ref cpu rs1) #:exhaustive])
      (bvand (bvnot (bv 1 (cpu-xlen cpu))) src)))
  (set-cpu-pc! cpu target))

; non-zero rs1/rd, zero rs2
(define-insn (nz-rs1/rd)
  #:encode (lambda (funct4 op)
                   (list (bv funct4 4) nz-rs1/rd (bv 0 5) (bv op 2)))
  [(#b1000 #b10) c.jr (interpret-cr-jmp #f)]
  [(#b1001 #b10) c.jalr (interpret-cr-jmp #t)])

; zero rs1/rd, zero rs2
(define-insn ()
  #:encode (lambda (funct4 op)
                   (list (bv funct4 4) (bv 0 5) (bv 0 5) (bv op 2)))
  [(#b1001 #b10) c.ebreak skip/debug])

;;; CI-type "Immediate" instructions

(define ((interpret-ci-type op) cpu insn imm5 rd imm4:0)
  (reg-imm-op op cpu insn (concat imm5 imm4:0) (decode-gpr rd) (decode-gpr rd)))

(define ((interpret-ci-w-type op) cpu insn imm5 rd imm4:0)
  (reg-imm-opw op cpu insn (concat imm5 imm4:0) (decode-gpr rd) (decode-gpr rd)))

(define ((interpret-stack-relative-load size) cpu insn imm5 nz-rd imm4:0)
  (define xlen (cpu-xlen cpu))
  (define mm (cpu-memmgr cpu))

  (core:bug-on (&& (= xlen 32) (= size 8))
               #:msg "c.ldsp: not available on rv32")

  (define off
    (zero-extend
      (cond
        [(= size 4)
          ; decode offset[5] and offset[4:2|7:6]
          (concat (extract 1 0 imm4:0)
                  (extract 0 0 imm5)
                  (extract 4 2 imm4:0)
                  (bv 0 2))]
        [(= size 8)
          ; decode offset[5] and offset[4:3|8:6]
          (concat (extract 2 0 imm4:0)
                  (extract 0 0 imm5)
                  (extract 4 3 imm4:0)
                  (bv 0 3))])
      (bitvector xlen)))

  (define addr (bvadd (gpr-ref cpu 'sp) off))
  (define value (core:memmgr-load mm addr (bv 0 xlen) (bv size xlen)))

  ; Always sign-extend.
  (set! value (sign-extend value (bitvector xlen)))

  (gpr-set! cpu (decode-gpr nz-rd) value)
  (cpu-next! cpu insn))

; non-zero rd
(define-insn (imm5 nz-rd imm4:0)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) imm5 nz-rd imm4:0 (bv op 2)))
  [(#b010 #b10) c.lwsp (interpret-stack-relative-load 4)]
  [(#b011 #b10) c.ldsp (interpret-stack-relative-load 8)]

  [(#b000 #b01) c.addi (interpret-ci-type bvadd)]
  [(#b010 #b01) c.li (interpret-ci-type (lambda (a b) b))]

  [(#b000 #b10) c.slli (interpret-shift-immediate bvshl decode-gpr)])

; c.addiw is rv64-only
(define-insn/64 (imm5 nz-rd imm4:0)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) imm5 nz-rd imm4:0 (bv op 2)))

  [(#b001 #b01) c.addiw (interpret-ci-w-type bvadd)])

(define (interpret-c.lui cpu insn nzimm17 c.lui-rd nzimm16:12)
  (define imm (concat nzimm17 nzimm16:12 (bv 0 12)))
  (core:bug-on (bvzero? imm) #:msg "c.lui: imm cannot be zero")
  (define op (lambda (a b) b))
  (define rd (decode-gpr c.lui-rd))
  (reg-imm-op op cpu insn imm rd rd))

; c.lui, rd != x0 /\ rd != x2
(define-insn (nzimm17 c.lui-rd nzimm16:12)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) nzimm17 c.lui-rd nzimm16:12 (bv op 2)))
  [(#b011 #b01) c.lui interpret-c.lui])

(define (interpret-c.addi16sp cpu insn nzimm9 nzimm4&6&8:7&5)
  (define xlen (cpu-xlen cpu))
  (define imm
    (sign-extend
      (concat (extract 0 0 nzimm9)
              (extract 2 1 nzimm4&6&8:7&5)
              (extract 3 3 nzimm4&6&8:7&5)
              (extract 0 0 nzimm4&6&8:7&5)
              (extract 4 4 nzimm4&6&8:7&5)
              (bv 0 4))
      (bitvector xlen)))

  (core:bug-on (bvzero? imm) #:msg "c.addi16sp: imm cannot be zero")

  (gpr-set! cpu 'sp (bvadd (gpr-ref cpu 'sp) imm))

  (cpu-next! cpu insn))

; c.addi16sp, rd = x2
(define-insn (nzimm9 nzimm4&6&8:7&5)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) nzimm9 (bv 2 5) nzimm4&6&8:7&5 (bv op 2)))
  [(#b011 #b01) c.addi16sp interpret-c.addi16sp])

; zero rd
(define-insn (imm5 imm4:0)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) imm5 (bv 0 5) imm4:0 (bv op 2)))
  [(#b000 #b01) c.nop skip/debug])

;;; CSS-type "Stack-relative Store" instructions

(define ((interpret-stack-relative-store size) cpu insn imm rs2)
  (set! rs2 (decode-gpr rs2))
  (define xlen (cpu-xlen cpu))
  (define mm (cpu-memmgr cpu))

  (define off
    (zero-extend
      (cond
        [(= size 4)
          ; decode uimm[5:2|7:6]
          (concat (extract 1 0 imm)
                  (extract 5 2 imm)
                  (bv 0 2))]
        [(= size 8)
          ; decode uimm[5:3|8:6]
          (concat (extract 2 0 imm)
                  (extract 5 3 imm)
                  (bv 0 3))])
      (bitvector xlen)))

  (define addr (bvadd (gpr-ref cpu 'sp) off))
  (define value (trunc (* 8 size) (gpr-ref cpu rs2)))

  (core:memmgr-store! mm addr (bv 0 xlen) value (bv size xlen))
  (cpu-next! cpu insn))

(define-insn (uimm5:2&7:6 rs2)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) uimm5:2&7:6 rs2 (bv op 2)))
  [(#b110 #b10) c.swsp (interpret-stack-relative-store 4)])

(define-insn/64 (uimm5:3&8:6 rs2)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) uimm5:3&8:6 rs2 (bv op 2)))
  [(#b111 #b10) c.sdsp (interpret-stack-relative-store 8)])

;;; CIW-type "Wide Immediate" Instructions

(define (interpret-c.addi4spn cpu insn nzuimm5:4&9:6&2&3 rd^)
  (define xlen (cpu-xlen cpu))
  (define rd (decode-compressed-gpr rd^))

  (define imm
    (zero-extend
      (concat (extract 5 2 nzuimm5:4&9:6&2&3)
              (extract 7 6 nzuimm5:4&9:6&2&3)
              (extract 0 0 nzuimm5:4&9:6&2&3)
              (extract 1 1 nzuimm5:4&9:6&2&3)
              (bv 0 2))
      (bitvector xlen)))

  (gpr-set! cpu rd (bvadd (gpr-ref cpu 'sp) imm))
  (cpu-next! cpu insn))

(define-insn (nzuimm5:4&9:6&2&3 rd^)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) nzuimm5:4&9:6&2&3 rd^ (bv op 2)))
  [(#b000 #b00) c.addi4spn interpret-c.addi4spn])

;;; CL-type "Load" instructions

(define ((interpret-cl-type size) cpu insn imm_hi rs1^ imm_lo rd^)
  (define xlen (cpu-xlen cpu))
  (define mm (cpu-memmgr cpu))
  (define rs (decode-compressed-gpr rs1^))
  (define rd (decode-compressed-gpr rd^))
  (define off
    (zero-extend
      (cond
        [(= size 4)
          ; decode offset[5:3] and offset[2|6]
          (concat (extract 0 0 imm_lo)
                  (extract 2 0 imm_hi)
                  (extract 1 1 imm_lo)
                  (bv 0 2))]
        [(= size 8)
          ; decode offset[5:3] and offset[7:6]
          (concat (extract 1 0 imm_lo)
                  (extract 2 0 imm_hi)
                  (bv 0 3))])
      (bitvector xlen)))

  (define addr (bvadd (gpr-ref cpu rs) off))
  (define value (core:memmgr-load mm addr (bv 0 xlen) (bv size xlen)))

  ; Always sign-extend.
  (set! value (sign-extend value (bitvector xlen)))

  (gpr-set! cpu rd value)
  (cpu-next! cpu insn))

(define-insn (uimm5:3 rs1^ uimm2&6 rd^)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) uimm5:3 rs1^ uimm2&6 rd^ (bv op 2)))
  [(#b010 #b00) c.lw (interpret-cl-type 4)])

(define-insn/64 (uimm5:3 rs1^ uimm7:6 rd^)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) uimm5:3 rs1^ uimm7:6 rd^ (bv op 2)))
  [(#b011 #b00) c.ld (interpret-cl-type 8)])

;;; CS-type "Store" instructions

(define ((interpret-cs-type size) cpu insn imm_hi rs1^ imm_lo rs2^)
  (define xlen (cpu-xlen cpu))
  (define mm (cpu-memmgr cpu))
  (define rs1 (decode-compressed-gpr rs1^))
  (define rs2 (decode-compressed-gpr rs2^))
  (define off
    (zero-extend
      (cond
        [(= size 4)
          ; decode offset[5:3] and offset[2|6]
          (concat (extract 0 0 imm_lo)
                  (extract 2 0 imm_hi)
                  (extract 1 1 imm_lo)
                  (bv 0 2))]
        [(= size 8)
          ; decode offset[5:3] and offset[7:6]
          (concat (extract 1 0 imm_lo)
                  (extract 2 0 imm_hi)
                  (bv 0 3))])
      (bitvector xlen)))

  (define addr (bvadd (gpr-ref cpu rs1) off))
  (define value (trunc (* 8 size) (gpr-ref cpu rs2)))

  (core:memmgr-store! mm addr (bv 0 xlen) value (bv size xlen))

  (cpu-next! cpu insn))

(define-insn (uimm5:3 rs1^ uimm2&6 rs2^)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) uimm5:3 rs1^ uimm2&6 rs2^ (bv op 2)))
  [(#b110 #b00) c.sw (interpret-cs-type 4)])

(define-insn/64 (uimm5:3 rs1^ uimm7:6 rs2^)
  #:encode (lambda (funct3 op)
                   (list (bv funct3 3) uimm5:3 rs1^ uimm7:6 rs2^ (bv op 2)))
  [(#b111 #b00) c.sd (interpret-cs-type 8)])

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
