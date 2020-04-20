#lang rosette

(require
  "common.rkt")

(provide
  pop-r)


; 58+rd
(define-insn pop-r (dst)
  #:decode [((+r #x58 reg))
            (list (gpr64-no-rex reg))]
  #:encode (list (+r #x58 dst))
  (lambda (cpu dst)
    (define mm (cpu-memmgr cpu))
    (define n (core:memmgr-bitwidth mm))
    (define size (bv (quotient n 8) n))
    (define sp (trunc n (cpu-gpr-ref cpu rsp)))
    (define v (core:memmgr-load mm sp (bv 0 n) size #:dbg (cpu-pc-ref cpu)))
    (cpu-gpr-set! cpu rsp (zero-extend (bvadd sp size) (bitvector 64)))
    (cpu-gpr-set! cpu dst (zero-extend v (bitvector 64)))))
