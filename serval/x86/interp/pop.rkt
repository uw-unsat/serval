#lang rosette

(require
  "common.rkt")

(provide
  pop-r pop-r64 leave)

(define (interpret-pop cpu dst)
  (define mm (cpu-memmgr cpu))
  (define n (core:memmgr-bitwidth mm))
  (define size (bv (quotient n 8) n))
  (define sp (trunc n (cpu-gpr-ref cpu rsp)))
  (define v (core:memmgr-load mm sp (bv 0 n) size))
  (cpu-gpr-set! cpu rsp (zero-extend (bvadd sp size) (bitvector 64)))
  (cpu-gpr-set! cpu dst (zero-extend v (bitvector 64))))

; 58+rd
(define-insn pop-r (dst)
  #:decode [((+r #x58 reg))
            (list (gpr64-no-rex reg))]
  #:encode (list (+r #x58 dst))
  interpret-pop)

; REX + 58+rd
(define-insn pop-r64 (dst)
  #:decode [((rex/r b) (+r #x58 reg))
            (list (gpr64 b reg))]
  #:encode (list (rex/r dst) (+r #x58 dst))
  interpret-pop)

; C9
(define-insn leave ()
  #:decode [((byte #xC9))
            (leave)]
  #:encode (list (byte #xC9))
  (lambda (cpu)
    (cpu-gpr-set! cpu rsp (cpu-gpr-ref cpu rbp))
    (interpret-pop cpu rbp)))