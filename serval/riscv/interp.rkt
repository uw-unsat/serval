#lang rosette

(require (prefix-in core: "../lib/core.rkt")
         (only-in racket/base hash-has-key? hash-ref)
         "../lib/memmgr.rkt"
         "../lib/bvarith.rkt"
         "base.rkt"
         "symopt.rkt"
         "interp/r-type.rkt"
         "interp/i-type.rkt"
         "interp/s-type.rkt"
         "interp/b-type.rkt"
         "interp/u-type.rkt"
         "interp/j-type.rkt"
         "interp/c-type.rkt")

(provide (all-defined-out)
         (all-from-out "base.rkt")
         (all-from-out "symopt.rkt")
         (all-from-out "interp/r-type.rkt")
         (all-from-out "interp/i-type.rkt")
         (all-from-out "interp/s-type.rkt")
         (all-from-out "interp/b-type.rkt")
         (all-from-out "interp/u-type.rkt")
         (all-from-out "interp/j-type.rkt")
         (all-from-out "interp/c-type.rkt"))

(struct program (base instructions) #:transparent)

(define (interpret-insn cpu insn)
  (instruction-run insn cpu))

(define (interpret-program cpu program)
  (define instructions (program-instructions program))
  (core:split-pc (cpu pc) cpu
    (define pc (cpu-pc cpu))
    (set-current-pc-debug! pc)
    (cond
      [(hash-has-key? (cpu-shims cpu) pc)
        ((hash-ref (cpu-shims cpu) pc) cpu)
        (interpret-program cpu program)]

      [(hash-has-key? instructions pc)
        (define insn (hash-ref instructions pc))
        (when (! (mret? insn))
          (interpret-insn cpu (hash-ref instructions pc))
          (interpret-program cpu program))]

      [else (core:bug #:msg (format "interpret-program: Unknown insn for pc: ~v" pc))])))
