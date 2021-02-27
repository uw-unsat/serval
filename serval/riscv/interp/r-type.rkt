#lang rosette

(require "common.rkt")

(provide (all-defined-out))

; Regular r-type insn.
(define ((interpret-r-type op) cpu insn rs2 rs1 rd)
  (reg-reg-op op cpu insn (decode-gpr rs2) (decode-gpr rs1) (decode-gpr rd)))

; 32-bit ops on rv64.
(define ((interpret-rw-type op) cpu insn rs2 rs1 rd)
  (reg-reg-opw op cpu insn (decode-gpr rs2) (decode-gpr rs1) (decode-gpr rd)))

(define ((interpret-amo-insn op size) cpu insn aq rl rs2 rs1 rd)
  (define mm (cpu-memmgr cpu))
  (define xlen (cpu-xlen cpu))
  (core:bug-on (&& (= size 8) (= xlen 32)) #:msg "no amo*.d on rv32")

  (core:memmgr-atomic-begin mm)

  (define addr (gpr-ref cpu (decode-gpr rs1)))
  (define oldvalue (core:memmgr-load mm addr (bv 0 xlen) (bv size xlen)))
  (gpr-set! cpu (decode-gpr rd) (sign-extend oldvalue (bitvector xlen)))
  (define newvalue (op oldvalue (trunc (* 8 size) (gpr-ref cpu (decode-gpr rs2)))))
  (core:memmgr-store! mm addr (bv 0 xlen) (trunc (* 8 size) newvalue) (bv size xlen))

  (core:memmgr-atomic-end mm)
  (cpu-next! cpu insn))

; R-type
(define-insn (rs2 rs1 rd)
  #:encode (lambda (funct7 funct3 opcode)
                   (list (bv funct7 7) rs2 rs1 (bv funct3 3) rd (bv opcode 7)))
  [(#b0000000 #b000 #b0110011) add (interpret-r-type bvadd)]
  [(#b0100000 #b000 #b0110011) sub (interpret-r-type bvsub)]
  [(#b0000000 #b001 #b0110011) sll (interpret-r-type (make-shift-op bvshl))]
  [(#b0000000 #b010 #b0110011) slt (interpret-r-type (make-cmp-op bvslt))]
  [(#b0000000 #b011 #b0110011) sltu (interpret-r-type (make-cmp-op bvult))]
  [(#b0000000 #b100 #b0110011) xor (interpret-r-type bvxor)]
  [(#b0000000 #b101 #b0110011) srl (interpret-r-type (make-shift-op bvlshr))]
  [(#b0100000 #b101 #b0110011) sra (interpret-r-type (make-shift-op bvashr))]
  [(#b0000000 #b110 #b0110011) or_ (interpret-r-type bvor)]
  [(#b0000000 #b111 #b0110011) and_ (interpret-r-type bvand)]

  ; RV64I Base Instruction Set (in addition to RV32I)
  [(#b0000000 #b000 #b0111011) addw (interpret-rw-type bvadd)]
  [(#b0100000 #b000 #b0111011) subw (interpret-rw-type bvsub)]
  [(#b0000000 #b001 #b0111011) sllw (interpret-rw-type (make-shift-op bvshl))]
  [(#b0000000 #b101 #b0111011) srlw (interpret-rw-type (make-shift-op bvlshr))]
  [(#b0100000 #b101 #b0111011) sraw (interpret-rw-type (make-shift-op bvashr))]

  ; RV32M Standard Extension
  [(#b0000001 #b000 #b0110011) mul (interpret-r-type (core:bvmul-proc))]
  [(#b0000001 #b001 #b0110011) mulh (interpret-r-type (core:bvmulh-proc))]
  [(#b0000001 #b010 #b0110011) mulhsu (interpret-r-type (core:bvmulhsu-proc))]
  [(#b0000001 #b011 #b0110011) mulhu (interpret-r-type (core:bvmulhu-proc))]
  [(#b0000001 #b100 #b0110011) div (interpret-r-type
    (lambda (v1 v2) (if (bvzero? v2) (bv -1 (core:bv-size v1)) ((core:bvsdiv-proc) v1 v2))))]
  [(#b0000001 #b101 #b0110011) divu (interpret-r-type (core:bvudiv-proc))]
  [(#b0000001 #b110 #b0110011) rem (interpret-r-type
    (lambda (v1 v2) (if (bvzero? v2) v1 ((core:bvsrem-proc) v1 v2))))]
  [(#b0000001 #b111 #b0110011) remu (interpret-r-type (core:bvurem-proc))]

  ; RV64M Standard Extension (in addition to RV32M)
  [(#b0000001 #b000 #b0111011) mulw (interpret-rw-type (core:bvmul-proc))]
  [(#b0000001 #b100 #b0111011) divw (interpret-rw-type
    (lambda (v1 v2) (if (bvzero? v2) (bv -1 (core:bv-size v1)) ((core:bvsdiv-proc) v1 v2))))]
  [(#b0000001 #b101 #b0111011) divuw (interpret-rw-type (core:bvudiv-proc))]
  [(#b0000001 #b110 #b0111011) remw (interpret-rw-type
    (lambda (v1 v2) (if (bvzero? v2) v1 ((core:bvsrem-proc) v1 v2))))]
  [(#b0000001 #b111 #b0111011) remuw (interpret-rw-type (core:bvurem-proc))]
)

; WFI: R-type with all concrete fields
(define-insn ()
  #:encode (lambda (funct7 rs2 rs1 funct3 rd opcode)
                   (list (bv funct7 7) (bv rs2 5) (bv rs1 5) (bv funct3 3) (bv rd 5) (bv opcode 7)))

  [(#b0000000 #b00010 #b00000 #b000 #b00000 #b1110011) uret notimplemented]
  [(#b0001000 #b00010 #b00000 #b000 #b00000 #b1110011) sret notimplemented]
  [(#b0011000 #b00010 #b00000 #b000 #b00000 #b1110011) mret notimplemented]

  [(#b0001000 #b00101 #b00000 #b000 #b00000 #b1110011) wfi skip]
)

; Fences: R-type with only rs2 and rs1
(define-insn (rs2 rs1)
  #:encode (lambda (funct7 funct3 rd opcode)
                   (list (bv funct7 7) rs2 rs1 (bv funct3 3) (bv rd 5) (bv opcode 7)))
  [(#b0001001 #b000 #b00000 #b1110011) sfence.vma skip]
  [(#b0010001 #b000 #b00000 #b1110011) hfence.vvma skip]
  [(#b0110001 #b000 #b00000 #b1110011) hfence.gvma skip])

; Atomic instructions
(define-insn (aq rl rs2 rs1 rd)
  #:encode (lambda (funct5 funct3 opcode)
                   (list (bv funct5 5) aq rl rs2 rs1 (bv funct3 3) rd (bv opcode 7)))
  [(#b00001 #b010 #b0101111) amoswap.w (interpret-amo-insn (lambda (v1 v2) v2) 4)]
  [(#b00000 #b010 #b0101111) amoadd.w (interpret-amo-insn bvadd 4)]
  [(#b00100 #b010 #b0101111) amoxor.w (interpret-amo-insn bvxor 4)]
  [(#b01100 #b010 #b0101111) amoand.w (interpret-amo-insn bvand 4)]
  [(#b01000 #b010 #b0101111) amoor.w (interpret-amo-insn bvor 4)]
  [(#b10000 #b010 #b0101111) amomin.w (interpret-amo-insn bvsmin 4)]
  [(#b10100 #b010 #b0101111) amomax.w (interpret-amo-insn bvsmax 4)]
  [(#b11000 #b010 #b0101111) amominu.w (interpret-amo-insn bvumin 4)]
  [(#b11100 #b010 #b0101111) amomaxu.w (interpret-amo-insn bvumax 4)]

  [(#b00001 #b011 #b0101111) amoswap.d (interpret-amo-insn (lambda (v1 v2) v2) 8)]
  [(#b00000 #b011 #b0101111) amoadd.d (interpret-amo-insn bvadd 8)]
  [(#b00100 #b011 #b0101111) amoxor.d (interpret-amo-insn bvxor 8)]
  [(#b01100 #b011 #b0101111) amoand.d (interpret-amo-insn bvand 8)]
  [(#b01000 #b011 #b0101111) amoor.d (interpret-amo-insn bvor 8)]
  [(#b10000 #b011 #b0101111) amomin.d (interpret-amo-insn bvsmin 8)]
  [(#b10100 #b011 #b0101111) amomax.d (interpret-amo-insn bvsmax 8)]
  [(#b11000 #b011 #b0101111) amominu.d (interpret-amo-insn bvumin 8)]
  [(#b11100 #b011 #b0101111) amomaxu.d (interpret-amo-insn bvumax 8)])