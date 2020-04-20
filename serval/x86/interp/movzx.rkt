#lang rosette

(require
  "common.rkt")

(provide
  movzx-r32-r/m8
  movzx-r64-r/m8
  movzx-r32-r/m16
  movzx-r64-r/m16)


(define (interpret-movzx cpu dst src n)
  (cpu-gpr-set! cpu dst (zero-extend (cpu-gpr-ref cpu src) (bitvector n))))

; 0F B6 /r
(define-insn movzx-r32-r/m8 (dst src)
  #:decode [((byte #x0F) (byte #xB6) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr8-no-rex r/m))]
           [((rex/r r b) (byte #x0F) (byte #xB6) (/r reg r/m))
            (list (gpr32 r reg) (gpr8 b r/m))]
  #:encode (list (rex/r dst src) (byte #x0F) (byte #xB6) (/r dst src))
  (lambda (cpu dst src)
    (interpret-movzx cpu dst src 32)))

(define-insn movzx-r32-m8 (dst src)
  #:decode [((byte #x0F) (byte #xB6) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) disp8 8))]
           [((byte #x0F) (byte #xB6) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 8))]
           [((rex/r r b) (byte #x0F) (byte #xB6) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr32 r reg) (register-indirect (gpr64 b r/m) disp8 8))]
           [((rex/r r b) (byte #x0F) (byte #xB6) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr32 r reg) (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 8))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex/r (car ed) (first es)) (byte #x0F) (byte #xB6) (modr/m (second es) (cdr ed) (third es)) (fourth es)))
  (lambda (cpu dst src)
    (interpret-movzx cpu dst src 32)))

; REX.W + 0F B6 /r
(define-insn movzx-r64-r/m8 (dst src)
  #:decode [((rex.w/r r b) (byte #x0F) (byte #xB6) (/r reg r/m))
            (list (gpr64 r reg) (gpr8 b r/m))]
  #:encode (list (rex.w/r dst src) (byte #x0F) (byte #xB6) (/r dst src))
  (lambda (cpu dst src)
    (interpret-movzx cpu dst src 64)))

(define-insn movzx-r64-m8 (dst src)
  #:decode [((rex.w/r r b) (byte #x0F) (byte #xB6) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr64 r reg) (register-indirect (gpr64 b r/m) disp8 8))]
           [((rex.w/r r b) (byte #x0F) (byte #xB6) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr64 r reg) (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 8))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex.w/r (car ed) (first es)) (byte #x0F) (byte #xB6) (modr/m (second es) (cdr ed) (third es)) (fourth es)))
  (lambda (cpu dst src)
    (interpret-movzx cpu dst src 64)))

; 0F B7 /r
(define-insn movzx-r32-r/m16 (dst src)
  #:decode [((byte #x0F) (byte #xB7) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr16-no-rex r/m))]
           [((rex/r r b) (byte #x0F) (byte #xB7) (/r reg r/m))
            (list (gpr32 r reg) (gpr16 b r/m))]
  #:encode (list (rex/r dst src) (byte #x0F) (byte #xB7) (/r dst src))
  (lambda (cpu dst src)
    (interpret-movzx cpu dst src 32)))

(define-insn movzx-r32-m16 (dst src)
  #:decode [((byte #x0F) (byte #xB7) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) disp8 16))]
           [((byte #x0F) (byte #xB7) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 16))]
           [((rex/r r b) (byte #x0F) (byte #xB7) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr32 r reg) (register-indirect (gpr64 b r/m) disp8 16))]
           [((rex/r r b) (byte #x0F) (byte #xB7) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr32 r reg) (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 16))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex/r (car ed) (first es)) (byte #x0F) (byte #xB7) (modr/m (second es) (cdr ed) (third es)) (fourth es)))
  (lambda (cpu dst src)
    (interpret-movzx cpu dst src 32)))

; REX.W + 0F B7 /r
(define-insn movzx-r64-r/m16 (dst src)
  #:decode [((rex.w/r r b) (byte #x0F) (byte #xB7) (/r reg r/m))
            (list (gpr64 r reg) (gpr16 b r/m))]
  #:encode (list (rex.w/r dst src) (byte #x0F) (byte #xB7) (/r dst src))
  (lambda (cpu dst src)
    (interpret-movzx cpu dst src 64)))

(define-insn movzx-r64-m16 (dst src)
  #:decode [((rex.w/r r b) (byte #x0F) (byte #xB7) (modr/m (== (bv #b01 2)) reg r/m) disp8)
            (list (gpr64 r reg) (register-indirect (gpr64 b r/m) disp8 16))]
           [((rex.w/r r b) (byte #x0F) (byte #xB7) (modr/m (== (bv #b10 2)) reg r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (gpr64 r reg) (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 16))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex.w/r (car ed) (first es)) (byte #x0F) (byte #xB7) (modr/m (second es) (cdr ed) (third es)) (fourth es)))
  (lambda (cpu dst src)
    (interpret-movzx cpu dst src 64)))
