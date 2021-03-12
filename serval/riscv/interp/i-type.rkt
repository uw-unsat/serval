#lang rosette

(require "common.rkt")

(provide (all-defined-out))

(define ((interpret-i-type op) cpu insn imm11:0 rs1 rd)
  (reg-imm-op op cpu insn imm11:0 (decode-gpr rs1) (decode-gpr rd)))

(define ((interpret-iw-type op) cpu insn imm11:0 rs1 rd)
  (reg-imm-opw op cpu insn imm11:0 (decode-gpr rs1) (decode-gpr rd)))

(define ((interpret-ishift-type op) cpu insn shamt rs1 rd)
  ; shamt is 6 bits: top bit must be 0 if rv32
  (core:bug-on (&& (= (cpu-xlen cpu) 32) (bvuge shamt (bv 32 6))) #:msg "32-bit shift immediate must be < 32")
  (define a (gpr-ref cpu (decode-gpr rs1)))
  (define b (zero-extend shamt (bitvector (cpu-xlen cpu))))
  (gpr-set! cpu (decode-gpr rd) (op a b))
  (cpu-next! cpu insn))

(define ((interpret-iwshift-type op) cpu insn shamt rs1 rd)
  (core:bug-on (! (= (cpu-xlen cpu) 64)) #:msg "*w: (cpu-xlen cpu) != 64")
  (define a (extract 31 0 (gpr-ref cpu (decode-gpr rs1))))
  (define b (zero-extend shamt (bitvector 32)))
  (gpr-set! cpu (decode-gpr rd) (sign-extend (op a b) (bitvector 64)))
  (cpu-next! cpu insn))

(define (interpret-jalr cpu insn imm11:0 rs1 rd)

  (define next (bvadd (cpu-pc cpu) (bv 4 (cpu-xlen cpu))))
  (gpr-set! cpu (decode-gpr rd) next)

  (define target
    (for/all ([src (gpr-ref cpu (decode-gpr rs1)) #:exhaustive])
      (bvand
        (bvnot (bv 1 (cpu-xlen cpu)))
        (bvadd src (sign-extend imm11:0 (bitvector (cpu-xlen cpu)))))))
  (set-cpu-pc! cpu target))

(define ((interpret-load-insn size signed?) cpu insn imm11:0 rs1 rd)
  (define mm (cpu-memmgr cpu))
  (define xlen (cpu-xlen cpu))

  (core:bug-on (&& (= xlen 32) (! signed?) (= size 4)) #:msg "no lwu on rv32")

  (define addr (bvadd (gpr-ref cpu (decode-gpr rs1))
                      (sign-extend imm11:0 (bitvector xlen))))

  (define value (core:memmgr-load mm addr (bv 0 xlen) (bv size xlen)))

  (set! value ((if signed? sign-extend zero-extend) value (bitvector xlen)))

  (gpr-set! cpu (decode-gpr rd) value)
  (cpu-next! cpu insn))

; Regular I-type
(define-insn (imm11:0 rs1 rd)
  #:encode (lambda (funct3 opcode)
                   (list imm11:0 rs1 (bv funct3 3) rd (bv opcode 7)))

  [(#b000 #b1100111) jalr interpret-jalr]

  [(#b000 #b0010011) addi (interpret-i-type bvadd)]
  [(#b010 #b0010011) slti (interpret-i-type (make-cmp-op bvslt))]
  [(#b011 #b0010011) sltiu (interpret-i-type (make-cmp-op bvult))]
  [(#b100 #b0010011) xori (interpret-i-type bvxor)]
  [(#b110 #b0010011) ori (interpret-i-type bvor)]
  [(#b111 #b0010011) andi (interpret-i-type bvand)]

  [(#b000 #b0011011) addiw (interpret-iw-type bvadd)]
  [(#b001 #b0001111) fence.i skip]

  [(#b000 #b0000011) lb (interpret-load-insn 1 #t)]
  [(#b001 #b0000011) lh (interpret-load-insn 2 #t)]
  [(#b010 #b0000011) lw (interpret-load-insn 4 #t)]
  [(#b100 #b0000011) lbu (interpret-load-insn 1 #f)]
  [(#b101 #b0000011) lhu (interpret-load-insn 2 #f)]

  [(#b011 #b0000011) ld (interpret-load-insn 8 #t)]
  [(#b110 #b0000011) lwu (interpret-load-insn 4 #f)])

(define ((do-csr-op op) cpu insn csr rs1 rd)
  (define xlen (cpu-xlen cpu))
  (set! csr (decode-csr csr))
  (set! rd (decode-gpr rd))
  (set! rs1 (decode-gpr rs1))

  (define old (zero-extend (csr-ref cpu csr) (bitvector xlen)))
  (define new (gpr-ref cpu rs1))

  (gpr-set! cpu rd old)

  (csr-set! cpu csr (op old new))
  (cpu-next! cpu insn))

; CSR instructions
(define-insn (csr rs1 rd)
  #:encode (lambda (funct3 opcode)
                   (list csr rs1 (bv funct3 3) rd (bv opcode 7)))
  ; RV32/RV64 Zicsr Standard Extension
  [(#b001 #b1110011) csrrw (do-csr-op (lambda (old new) new))]
  [(#b010 #b1110011) csrrs (do-csr-op (lambda (old new) (bvor old new)))]
  [(#b011 #b1110011) csrrc (do-csr-op (lambda (old new) (bvand old (bvnot new))))])

(define ((do-csr-op-imm op) cpu insn csr uimm rd)
  (define xlen (cpu-xlen cpu))
  (set! csr (decode-csr csr))
  (set! rd (decode-gpr rd))

  (define old (zero-extend (csr-ref cpu csr) (bitvector xlen)))
  (define new (zero-extend uimm (bitvector xlen)))

  (gpr-set! cpu rd old)

  (csr-set! cpu csr (op old new))
  (cpu-next! cpu insn))

; CSRI instructions
(define-insn (csr uimm rd)
  #:encode (lambda (funct3 opcode)
                   (list csr uimm (bv funct3 3) rd (bv opcode 7)))
  ; RV32/RV64 Zicsr Standard Extension
  [(#b101 #b1110011) csrrwi (do-csr-op-imm (lambda (old new) new))]
  [(#b110 #b1110011) csrrsi (do-csr-op-imm (lambda (old new) (bvor old new)))]
  [(#b111 #b1110011) csrrci (do-csr-op-imm (lambda (old new) (bvand old (bvnot new))))])

; 64-bit Shift instructions
(define-insn (shamt6 rs1 rd)
  #:encode (lambda (funct6 funct3 opcode)
                   (list (bv funct6 6) shamt6 rs1 (bv funct3 3) rd (bv opcode 7)))
  [(#b000000 #b001 #b0010011) slli (interpret-ishift-type bvshl)]
  [(#b000000 #b101 #b0010011) srli (interpret-ishift-type bvlshr)]
  [(#b010000 #b101 #b0010011) srai (interpret-ishift-type bvashr)])

; 32-bit shift instructions
(define-insn (shamt5 rs1 rd)
  #:encode (lambda (funct7 funct3 opcode)
                   (list (bv funct7 7) shamt5 rs1 (bv funct3 3) rd (bv opcode 7)))
  [(#b0000000 #b001 #b0011011) slliw (interpret-iwshift-type bvshl)]
  [(#b0000000 #b101 #b0011011) srliw (interpret-iwshift-type bvlshr)]
  [(#b0100000 #b101 #b0011011) sraiw (interpret-iwshift-type bvashr)])

; FENCE is a weird I-type instruction
(define-insn (fm pred succ rs1 rd)
  #:encode (lambda (funct3 opcode)
                   (list fm pred succ rs1 (bv funct3 3) rd (bv opcode 7)))
  [(#b000 #b0001111) fence skip])
