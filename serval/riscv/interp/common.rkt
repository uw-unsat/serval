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

(define (cpu-next! cpu size)
  (set-cpu-pc! cpu (bvadd (bv size (cpu-xlen cpu)) (cpu-pc cpu))))

; remove once all are implemented
(define (notimplemented cpu . args)
  (error "not implemented"))

(define (skip4 cpu . args)
  (cpu-next! cpu 4))

; Make a shift op from SMT shift operation by masking out upper bits.
(define ((make-shift-op op) v1 v2)
  (op v1 (bvand (bv (sub1 (core:bv-size v1)) (core:bv-size v1)) v2)))

; Make a comparison op from SMT comparison and lifting to bv.
(define ((make-cmp-op op) v1 v2)
  (if (op v1 v2) (bv 1 (core:bv-size v1)) (bv 0 (core:bv-size v1))))
