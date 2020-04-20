#lang rosette

(require
  "common.rkt")

(provide
  bswap-r32
  bswap-r64)


(define (interpret-bswap cpu dst)
  (define v (cpu-gpr-ref cpu dst))
  (define result (core:list->bitvector/le (reverse (core:bitvector->list/le v))))
  (cpu-gpr-set! cpu dst result))

; 0F C8+rd
(define-insn bswap-r32 (dst)
  #:decode [((byte #x0F) (+r #xC8 reg))
            (list (gpr32-no-rex reg))]
           [((rex/r b) (byte #x0F) (+r #xC8 reg))
            (list (gpr32 b reg))]
  #:encode (list (rex/r dst) (byte #x0F) (+r #xC8 dst))
  interpret-bswap)

; REX.W + 0F C8+rd
(define-insn bswap-r64 (dst)
  #:decode [((rex.w/r b) (byte #x0F) (+r #xC8 reg))
            (list (gpr64 b reg))]
  #:encode (list (rex.w/r dst) (byte #x0F) (+r #xC8 dst))
  interpret-bswap)
