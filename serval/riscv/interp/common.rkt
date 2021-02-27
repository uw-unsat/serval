#lang rosette

(require
  "define.rkt"
  "../base.rkt"
  "../../lib/bvarith.rkt"
  (prefix-in core: "../../lib/core.rkt"))

(provide
  (all-defined-out)
  (all-from-out
    "define.rkt"
    "../base.rkt"
    "../../lib/bvarith.rkt"
    "../../lib/core.rkt"))

(struct ptr (addr off size) #:transparent)

(define (notimplemented cpu insn . args)
  (error (format "instruction ~v not implemented" insn)))

(define (cpu-next! cpu insn)
  (set-cpu-pc! cpu (bvadd (bv (instruction-size insn) (cpu-xlen cpu))
                          (cpu-pc cpu))))

(define (skip cpu insn . args)
  (cpu-next! cpu insn))

(define (skip/debug cpu insn . args)
  (displayln insn)
  (cpu-next! cpu insn))

; Make a shift op from SMT shift operation by masking out upper bits.
(define ((make-shift-op op) v1 v2)
  (op v1 (bvand (bv (sub1 (core:bv-size v1)) (core:bv-size v1)) v2)))

; Make a comparison op from SMT comparison and lifting to bv.
(define ((make-cmp-op op) v1 v2)
  (if (op v1 v2) (bv 1 (core:bv-size v1)) (bv 0 (core:bv-size v1))))

; Register-register operation
(define (reg-reg-op op cpu insn rs2 rs1 rd)
  (define a (gpr-ref cpu rs1))
  (define b (gpr-ref cpu rs2))
  (gpr-set! cpu rd (op a b))
  (cpu-next! cpu insn))

(define (reg-imm-op op cpu insn imm rs1 rd)
  (define xlen (cpu-xlen cpu))
  (define a (gpr-ref cpu rs1))
  (define b (sign-extend imm (bitvector xlen)))
  (gpr-set! cpu rd (op a b))
  (cpu-next! cpu insn))

; 32-bit ops on rv64
(define (reg-reg-opw op cpu insn rs2 rs1 rd)
  (core:bug-on (! (= (cpu-xlen cpu) 64)) #:msg (format "~v: (cpu-xlen cpu) != 64" insn))
  (define a (trunc 32 (gpr-ref cpu rs1)))
  (define b (trunc 32 (gpr-ref cpu rs2)))
  (gpr-set! cpu rd (sign-extend (op a b) (bitvector 64)))
  (cpu-next! cpu insn))

(define (reg-imm-opw op cpu insn imm rs1 rd)
  (core:bug-on (! (= (cpu-xlen cpu) 64)) #:msg (format "~v: (cpu-xlen cpu) != 64" insn))
  (define a (trunc 32 (gpr-ref cpu rs1)))
  (define b (sign-extend imm (bitvector 32)))
  (gpr-set! cpu rd (sign-extend (op a b) (bitvector 64)))
  (cpu-next! cpu insn))
