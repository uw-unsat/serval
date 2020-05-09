#lang rosette

(require
  "common.rkt")

(provide
  jmp-rel8
  jmp-rel32
  jmp-r/m64-no-rex)


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


; FF /4
(define-insn jmp-r/m64-no-rex (src)
  #:decode [((byte #xFF) (/4 r/m))
            (list (gpr64-no-rex r/m))]
  #:encode (list (byte #xFF) (/4 src))
  (lambda (cpu src)
    ; deduct the instruction length for interpret-insn
    (cpu-pc-set! cpu (bvsub (cpu-gpr-ref cpu src) (bv 2 64)))))
