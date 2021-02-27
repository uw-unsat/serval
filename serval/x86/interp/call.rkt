#lang rosette

(require
  "common.rkt")

(provide
  call-rel32)


; E8 cd
(define (interpret-relative-call cpu rel)
  (define mm (cpu-memmgr cpu))
  (define n (core:memmgr-bitwidth mm))
  (define size (bv (quotient n 8) n))
  (define pc (cpu-pc-ref cpu))

  ; Add 5 bytes for this instruction
  (define ret-addr (bvadd pc (bv 5 (type-of pc))))

  ; Push return address to stack
  (define sp (bvsub (trunc n (cpu-gpr-ref cpu rsp)) size))
  (cpu-gpr-set! cpu rsp (zero-extend sp (bitvector 64)))
  (core:memmgr-store! mm sp (bv 0 n) ret-addr size)

  ; Jump to offset
  (cpu-pc-next! cpu (sign-extend rel (bitvector 64))))

(define-insn call-rel32 (rel32)
  #:decode [((byte #xE8) i0 i1 i2 i3)
            (call-rel32 (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #xE8) (encode-imm rel32))
  (lambda (cpu rel32)
    (interpret-relative-call cpu rel32)))
