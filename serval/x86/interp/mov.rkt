#lang rosette

(require
  "common.rkt")

(provide
  mov-r/m8-r8
  mov-r/m16-r16
  mov-r/m32-r32
  mov-r/m64-r64
  mov-r32-r/m32
  mov-r64-r/m64
  mov-r8-imm8
  mov-r8*-imm8
  mov-r32-imm32
  mov-r64-imm64
  mov-r/m8-imm8
  mov-r/m16-imm16
  mov-r/m32-imm32
  mov-r/m64-imm32)


; 88 /r
(define-insn mov-r/m8-r8 (dst src)
  #:decode [((byte #x88) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64-no-rex r/m) disp8 8) (gpr8-no-rex reg))]
           [((byte #x88) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 8) (gpr8-no-rex reg))]
           [((rex/r r b) (byte #x88) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64 b r/m) disp8 8) (gpr8 r reg))]
           [((rex/r r b) (byte #x88) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 8) (gpr8 r reg))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex/r (car es) (first ed)) (byte #x88) (modr/m (second ed) (cdr es) (third ed)) (fourth ed)))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

; 89 /r
(define-insn mov-r/m16-r16 (dst src)
  #:decode [((byte #x66) (byte #x89) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64-no-rex r/m) disp8 16) (gpr16-no-rex reg))]
           [((byte #x66) (byte #x89) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 16) (gpr16-no-rex reg))]
           [((byte #x66) (rex/r r b) (byte #x89) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64 b r/m) disp8 16) (gpr16 r reg))]
           [((byte #x66) (rex/r r b) (byte #x89) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 16) (gpr16 r reg))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (byte #x66) (rex/r (car es) (first ed)) (byte #x89) (modr/m (second ed) (cdr es) (third ed)) (fourth ed)))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

; 89 /r
(define-insn mov-r/m32-r32 (dst src)
  #:decode [((byte #x89) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x89) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x89) (/r src dst))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

(define-insn mov-m32-r32 (dst src)
  #:decode [((byte #x89) (modr/m (== (bv #b00 2)) reg r/m))
            (list (register-indirect (gpr64-no-rex r/m) #f 32) (gpr32-no-rex reg))]
           [((byte #x89) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64-no-rex r/m) disp8 32) (gpr32-no-rex reg))]
           [((byte #x89) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x89) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64 b r/m) disp8 32) (gpr32 r reg))]
           [((rex/r r b) (byte #x89) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32) (gpr32 r reg))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex/r (car es) (first ed)) (byte #x89) (modr/m (second ed) (cdr es) (third ed)) (fourth ed)))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

; REX.W + 89 /r
(define-insn mov-r/m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x89) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x89) (/r src dst))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

(define-insn mov-m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x89) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (register-indirect (gpr64 b r/m) disp8 64) (gpr64 r reg))]
           [((rex.w/r r b) (byte #x89) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 64) (gpr64 r reg))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex.w/r (car es) (first ed)) (byte #x89) (modr/m (second ed) (cdr es) (third ed)) (fourth ed)))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

; 8B /r
(define-insn mov-r32-r/m32 (dst src)
  #:decode [((byte #x8B) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr32-no-rex r/m))]
           [((rex/r r b) (byte #x8B) (/r reg r/m))
            (list (gpr32 r reg) (gpr32 b r/m))]
  #:encode (list (rex/r dst src) (byte #x8B) (/r dst src))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

(define-insn mov-r32-m32 (dst src)
  #:decode [((byte #x8B) (modr/m (== (bv #b00 2)) reg r/m))
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) #f 32))]
           [((byte #x8B) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) disp8 32))]
           [((byte #x8B) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32))]
           [((rex/r r b) (byte #x8B) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr32 r reg) (register-indirect (gpr64 b r/m) disp8 32))]
           [((rex/r r b) (byte #x8B) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr32 r reg) (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex/r (car ed) (first es)) (byte #x8B) (modr/m (second es) (cdr ed) (third es)) (fourth es)))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

; REX.W 8B /r
(define-insn mov-r64-r/m64 (dst src)
  #:decode [((rex.w/r r b) (byte #x8B) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr64 r reg) (register-indirect (gpr64 b r/m) disp8 64))]
           [((rex.w/r r b) (byte #x8B) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr64 r reg) (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 64))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex.w/r (car ed) (first es)) (byte #x8B) (modr/m (second es) (cdr ed) (third es)) (fourth es)))
  (lambda (cpu dst src)
    (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))))

; B0+ rb ib
(define-insn mov-r8-imm8 (dst imm8)
  #:decode [((+r #xB0 reg) ib)
            (list (gpr8-no-rex reg) ib)]
           [((rex/r b) (+r #xB0 reg) ib)
            (list (gpr8 b reg) ib)]
  #:encode (list (rex/r dst) (+r #xB0 dst) imm8)
  (lambda (cpu dst imm8)
    (cpu-gpr-set! cpu dst imm8)))

; REX.W + B0+ rb ib
(define-insn mov-r8*-imm8 (dst imm8)
  #:decode [((rex.w/r b) (+r #xB0 reg) ib)
            (list (gpr8 b reg) ib)]
  #:encode (list (rex.w/r dst) (+r #xB0 dst) imm8)
  (lambda (cpu dst imm8)
    (cpu-gpr-set! cpu dst imm8)))

; B8+ rd id
(define-insn mov-r32-imm32 (dst imm32)
  #:decode [((+r #xB8 reg) i0 i1 i2 i3)
            (list (gpr32-no-rex reg) (decode-imm i0 i1 i2 i3))]
           [((rex/r b) (+r #xB8 reg) i0 i1 i2 i3)
            (list (gpr32 b reg) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex/r dst) (+r #xB8 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (cpu-gpr-set! cpu dst imm32)))

; REX.W + B8+ rd io
(define-insn mov-r64-imm64 (dst imm64)
  #:decode [((rex.w/r b) (+r #xB8 reg) i0 i1 i2 i3 i4 i5 i6 i7)
            (list (gpr64 b reg) (decode-imm i0 i1 i2 i3 i4 i5 i6 i7))]
  #:encode (list (rex.w/r dst) (+r #xB8 dst) (encode-imm imm64))
  (lambda (cpu dst imm64)
    (cpu-gpr-set! cpu dst imm64)))

; C6 /0 ib
(define-insn mov-r/m8-imm8 (dst imm8)
  #:decode [((byte #xC6) (modr/m (== (bv #b01 2)) (== (bv 0 3)) r/m) disp8 ib)
            (list (register-indirect (gpr64-no-rex r/m) disp8 8) ib)]
           [((byte #xC6) (modr/m (== (bv #b10 2)) (== (bv 0 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3 ib)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 8) ib)]
           [((rex/r b) (byte #xC6) (modr/m (== (bv #b01 2)) (== (bv 0 3)) r/m) disp8 ib)
            (list (register-indirect (gpr64 b r/m) disp8 8) ib)]
           [((rex/r b) (byte #xC6) (modr/m (== (bv #b10 2)) (== (bv 0 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3 ib)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 8) ib)]
  #:encode (let ([e (register-encode dst)])
             (list (rex/r (first e)) (byte #xC6) (modr/m (second e) (bv 0 3) (third e)) (fourth e) imm8))
  (lambda (cpu dst imm8)
    (cpu-gpr-set! cpu dst imm8)))

; C7 /0 iw
(define-insn mov-r/m16-imm16 (dst imm16)
  #:decode [((byte #x66) (byte #xC7) (modr/m (== (bv #b01 2)) (== (bv 0 3)) r/m) disp8 iw.0 iw.1)
            (list (register-indirect (gpr64-no-rex r/m) disp8 16) (decode-imm iw.0 iw.1))]
           [((byte #x66) (byte #xC7) (modr/m (== (bv #b10 2)) (== (bv 0 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3 iw.0 iw.1)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 16) (decode-imm iw.0 iw.1))]
           [((byte #x66) (rex/r b) (byte #xC7) (modr/m (== (bv #b01 2)) (== (bv 0 3)) r/m) disp8 iw.0 iw.1)
            (list (register-indirect (gpr64 b r/m) disp8 16) (decode-imm iw.0 iw.1))]
           [((byte #x66) (rex/r b) (byte #xC7) (modr/m (== (bv #b10 2)) (== (bv 0 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3 iw.0 iw.1)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 16) (decode-imm iw.0 iw.1))]
  #:encode (let ([e (register-encode dst)])
             (list (byte #x66) (rex/r (first e)) (byte #xC7) (modr/m (second e) (bv 0 3) (third e)) (fourth e) (encode-imm imm16)))
  (lambda (cpu dst imm16)
    (cpu-gpr-set! cpu dst imm16)))

; C7 /0 id
(define-insn mov-r/m32-imm32 (dst imm32)
  #:decode [((byte #xC7) (/0 r/m) id.0 id.1 id.2 id.3)
            (list (gpr32-no-rex r/m) (decode-imm id.0 id.1 id.2 id.3))]
           [((rex/r b) (byte #xC7) (/0 r/m) id.0 id.1 id.2 id.3)
            (list (gpr32 b r/m) (decode-imm id.0 id.1 id.2 id.3))]
  #:encode (list (rex/r dst) (byte #xC7) (/0 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (cpu-gpr-set! cpu dst imm32)))

(define-insn mov-m32-imm32 (dst imm32)
  #:decode [((byte #xC7) (modr/m (== (bv #b01 2)) (== (bv 0 3)) r/m) disp8 id.0 id.1 id.2 id.3)
            (list (register-indirect (gpr64-no-rex r/m) disp8 32) (decode-imm id.0 id.1 id.2 id.3))]
           [((byte #xC7) (modr/m (== (bv #b10 2)) (== (bv 0 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3 id.0 id.1 id.2 id.3)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32) (decode-imm id.0 id.1 id.2 id.3))]
           [((rex/r b) (byte #xC7) (modr/m (== (bv #b01 2)) (== (bv 0 3)) r/m) disp8 id.0 id.1 id.2 id.3)
            (list (register-indirect (gpr64 b r/m) disp8 32) (decode-imm id.0 id.1 id.2 id.3))]
           [((rex/r b) (byte #xC7) (modr/m (== (bv #b10 2)) (== (bv 0 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3 id.0 id.1 id.2 id.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32) (decode-imm id.0 id.1 id.2 id.3))]
  #:encode (let ([e (register-encode dst)])
             (list (rex/r (first e)) (byte #xC7) (modr/m (second e) (bv 0 3) (third e)) (fourth e) (encode-imm imm32)))
  (lambda (cpu dst imm32)
    (cpu-gpr-set! cpu dst imm32)))

; REX.W + C7 /0 id
(define-insn mov-r/m64-imm32 (dst imm32)
  #:decode [((rex.w/r b) (byte #xC7) (/0 r/m) i0 i1 i2 i3)
            (list (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r dst) (byte #xC7) (/0 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (cpu-gpr-set! cpu dst (sign-extend imm32 (bitvector 64)))))

(define-insn mov-m64-imm32 (dst imm32)
  #:decode [((rex.w/r b) (byte #xC7) (modr/m (== (bv #b01 2)) (== (bv 0 3)) r/m) disp8 id.0 id.1 id.2 id.3)
            (list (register-indirect (gpr64 b r/m) disp8 64) (decode-imm id.0 id.1 id.2 id.3))]
           [((rex.w/r b) (byte #xC7) (modr/m (== (bv #b10 2)) (== (bv 0 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3 id.0 id.1 id.2 id.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 64) (decode-imm id.0 id.1 id.2 id.3))]
  #:encode (let ([e (register-encode dst)])
             (list (rex.w/r (first e)) (byte #xC7) (modr/m (second e) (bv 0 3) (third e)) (fourth e) (encode-imm imm32)))
  (lambda (cpu dst imm32)
    (cpu-gpr-set! cpu dst (sign-extend imm32 (bitvector 64)))))
