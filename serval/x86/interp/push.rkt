#lang rosette

(require
  "common.rkt")

(provide
  push-r push-r64)

(define (interpret-push cpu src)
  (define mm (cpu-memmgr cpu))
  (define n (core:memmgr-bitwidth mm))
  (define size (bv (quotient n 8) n))
  (define v (trunc n (cpu-gpr-ref cpu src)))
  (define sp (bvsub (trunc n (cpu-gpr-ref cpu rsp)) size))
  (cpu-gpr-set! cpu rsp (zero-extend sp (bitvector 64)))
  (core:memmgr-store! mm sp (bv 0 n) v size))

; 50+rd
(define-insn push-r (src)
  #:decode [((+r #x50 reg))
            (list (gpr64-no-rex reg))]
  #:encode (list (+r #x50 src))
  interpret-push)

; REX + 50+rd
(define-insn push-r64 (src)
  #:decode [((rex/r b) (+r #x50 src))
            (list (gpr64 b src))]
  #:encode (list (rex/r src) (+r #x50 src))
  interpret-push)
