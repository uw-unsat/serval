#lang rosette

(require
  "define.rkt"
  "../base.rkt"
  (prefix-in core: "../../lib/core.rkt"))

(provide
  (all-defined-out)
  (all-from-out
    "define.rkt"
    "../base.rkt"
    "../../lib/core.rkt"))

(struct ptr (addr off size) #:transparent)

(define (notimplemented cpu insn . args)
  (error (format "instruction ~v not implemented" insn)))

(define (instruction-size insn)
  (for/all ([insn insn #:exhaustive])
    (/ (core:bv-size (instruction-encode insn)) 8)))

(define (cpu-next! cpu insn)
  (set-cpu-pc! cpu (bvadd (bv (instruction-size insn) (cpu-xlen cpu))
                          (cpu-pc cpu))))

(define (skip cpu insn . args)
  (cpu-next! cpu insn))

; Make a shift op from SMT shift operation by masking out upper bits.
(define ((make-shift-op op) v1 v2)
  (op v1 (bvand (bv (sub1 (core:bv-size v1)) (core:bv-size v1)) v2)))

; Make a comparison op from SMT comparison and lifting to bv.
(define ((make-cmp-op op) v1 v2)
  (if (op v1 v2) (bv 1 (core:bv-size v1)) (bv 0 (core:bv-size v1))))
