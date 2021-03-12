#lang rosette

(require
  "common.rkt")

(provide
  ret-near)


; C3
(define (interpret-return cpu)
  (define mm (cpu-memmgr cpu))
  (define n (core:memmgr-bitwidth mm))
  (define size (bv (quotient n 8) n))
  (define pc (cpu-pc-ref cpu))

  ; Load return address
  (define ret-addr
    (core:memmgr-load mm (trunc n (cpu-gpr-ref cpu rsp)) (bv 0 n) size))

  ; Pop addr from stack
  (define sp (bvadd (trunc n (cpu-gpr-ref cpu rsp)) size))
  (cpu-gpr-set! cpu rsp (zero-extend sp (bitvector 64)))

  ; Jump to return address
  ; Subtract 1 to adjust for cpu-next! in interpret-insn
  (set-cpu-pc! cpu (bvsub1 ret-addr)))

(define-insn ret-near ()
  #:decode [((byte #xC3))
            (ret-near)]
  #:encode (list (byte #xC3))
  (lambda (cpu)
    (interpret-return cpu)))
