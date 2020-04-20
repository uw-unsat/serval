#lang rosette

(require
  "common.rkt")

(provide
  add-eax-imm32
  add-rax-imm32
  add-r/m32-imm32
  add-r/m64-imm32
  add-r/m32-imm8
  add-r/m64-imm8
  add-r/m32-r32
  add-r/m64-r64)


(define (interpret-add cpu dst v2)
  (define v1 (cpu-gpr-ref cpu dst))
  (define result (bvadd v1 v2))
  (cpu-gpr-set! cpu dst result)
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-set! cpu 'CF (bool->bitvector (core:bvuadd-overflow? v1 v2)))
  (cpu-flag-set! cpu 'OF (bool->bitvector (core:bvsadd-overflow? v1 v2)))
  (cpu-flag-set! cpu 'AF (bool->bitvector (core:bvuadd-overflow? (trunc 4 v1) (trunc 4 v2)))))

; 05 id
(define-insn add-eax-imm32 (imm32)
  #:decode [((byte #x05) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #x05) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-add cpu eax imm32)))

; REX.W + 05 id
(define-insn add-rax-imm32 (imm32)
  #:decode [((rex.w/r) (byte #x05) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r) (byte #x05) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-add cpu rax (sign-extend imm32 (bitvector 64)))))

; 81 /0 id
(define-insn add-r/m32-imm32 (dst imm32)
  #:decode [((byte #x81) (/0 r/m) i0 i1 i2 i3)
            (list (gpr32-no-rex r/m) (decode-imm i0 i1 i2 i3))]
           [((rex/r b) (byte #x81) (/0 r/m) i0 i1 i2 i3)
            (list (gpr32 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex/r dst) (byte #x81) (/0 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-add cpu dst imm32)))

; REX.W + 81 /0 id
(define-insn add-r/m64-imm32 (dst imm32)
  #:decode [((rex.w/r b) (byte #x81) (/0 r/m) i0 i1 i2 i3)
            (list (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r dst) (byte #x81) (/0 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-add cpu dst (sign-extend imm32 (bitvector 64)))))

; 83 /0 ib
(define-insn add-r/m32-imm8 (dst imm8)
  #:decode [((byte #x83) (/0 r/m) i0)
            (list (gpr32-no-rex r/m) i0)]
           [((rex/r b) (byte #x83) (/0 r/m) i0)
            (list (gpr32 b r/m) i0)]
  #:encode (list (rex/r dst) (byte #x83) (/0 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-add cpu dst (sign-extend imm8 (bitvector 32)))))

; REX.W + 83 /0 ib
(define-insn add-r/m64-imm8 (dst imm8)
  #:decode [((rex.w/r b) (byte #x83) (/0 r/m) i0)
            (list (gpr64 b r/m) i0)]
  #:encode (list (rex.w/r dst) (byte #x83) (/0 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-add cpu dst (sign-extend imm8 (bitvector 64)))))

; 01 /r
(define-insn add-r/m32-r32 (dst src)
  #:decode [((byte #x01) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x01) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x01) (/r src dst))
  (lambda (cpu dst src)
    (interpret-add cpu dst (cpu-gpr-ref cpu src))))

; REX.W + 01 /r
(define-insn add-r/m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x01) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x01) (/r src dst))
  (lambda (cpu dst src)
    (interpret-add cpu dst (cpu-gpr-ref cpu src))))

; LOCK prefix for ADD only for now

(define-insn lock-add-r/m32-r32 (dst src)
  #:decode [((byte #xF0) (byte #x01) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64-no-rex r/m) disp8 32) (gpr32-no-rex reg))]
           [((byte #xF0) (byte #x01) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32) (gpr32-no-rex reg))]
           [((byte #xF0) (rex/r r b) (byte #x01) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64 b r/m) disp8 32) (gpr32 r reg))]
           [((byte #xF0) (rex/r r b) (byte #x01) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32) (gpr32 r reg))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (byte #xF0) (rex/r (car es) (first ed)) (byte #x01) (modr/m (second ed) (cdr es) (third ed)) (fourth ed)))
  (lambda (cpu dst src)
    (define mm (cpu-memmgr cpu))
    (core:memmgr-atomic-begin mm)
    (interpret-add cpu dst (cpu-gpr-ref cpu src))
    (core:memmgr-atomic-end mm)))

(define-insn lock-add-r/m64-r64 (dst src)
  #:decode [((byte #xF0) (rex.w/r r b) (byte #x01) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64 b r/m) disp8 64) (gpr64 r reg))]
           [((byte #xF0) (rex.w/r r b) (byte #x01) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 64) (gpr64 r reg))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (byte #xF0) (rex.w/r (car es) (first ed)) (byte #x01) (modr/m (second ed) (cdr es) (third ed)) (fourth ed)))
  (lambda (cpu dst src)
    (define mm (cpu-memmgr cpu))
    (core:memmgr-atomic-begin mm)
    (interpret-add cpu dst (cpu-gpr-ref cpu src))
    (core:memmgr-atomic-end mm)))
