#lang rosette

(require "base.rkt"
         (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out))

; Jalr masks out the lowest bit of the PC.
; This symbolic optimizations kills the mask and emits an assertion that
; the rewrite is sound; to be used in situations where the target is known
; to be aligned and the resulting PC must be as concrete as possible.
(define (kill-jalr-mask cpu)
  (define mask (bvnot (bv 1 (cpu-xlen cpu))))
  (define newpc
    (match (cpu-pc cpu)
      [(expression (== bvand) x y) #:when (and (! (term? x)) (equal? x mask)) y]
      [pc pc]))
  (core:bug-on (! (equal? newpc (cpu-pc cpu)))
               #:msg "kill-jalr-mask: symbolic optimization must be sound")
  (set-cpu-pc! cpu newpc))
