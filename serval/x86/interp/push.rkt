#lang rosette

(require
  "common.rkt")

(provide
  push-r)


; 50+rd
(define-insn push-r (src)
  #:decode [((+r #x50 reg))
            (list (gpr64-no-rex reg))]
  #:encode (list (+r #x50 src))
  (lambda (cpu src)
    (define mm (cpu-memmgr cpu))
    (define n (core:memmgr-bitwidth mm))
    (define size (bv (quotient n 8) n))
    (define v (trunc n (cpu-gpr-ref cpu src)))
    (define sp (bvsub (trunc n (cpu-gpr-ref cpu rsp)) size))
    (cpu-gpr-set! cpu rsp (zero-extend sp (bitvector 64)))
    (core:memmgr-store! mm sp (bv 0 n) v size #:dbg (cpu-pc-ref cpu))))
