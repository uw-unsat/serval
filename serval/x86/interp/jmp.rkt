#lang rosette

(require
  "common.rkt")

(provide
  jmp-rel8
  jmp-rel32)


; EB cb
(define-insn jmp-rel8 (rel8)
  #:decode [((byte #xEB) i0)
            (list (decode-imm i0))]
  #:encode (list (byte #xEB) (encode-imm rel8))
  (lambda (cpu rel8)
    (cpu-pc-next! cpu (sign-extend rel8 (bitvector 64)))))


; E9 cd
(define-insn jmp-rel32 (rel32)
  #:decode [((byte #xE9) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #xE9) (encode-imm rel32))
  (lambda (cpu rel32)
    (cpu-pc-next! cpu (sign-extend rel32 (bitvector 64)))))
