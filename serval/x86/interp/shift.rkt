#lang rosette

(require
  "common.rkt")

(provide
  sar-r/m32-1 sar-r/m64-1
  shl-r/m32-1 shl-r/m64-1
  shr-r/m32-1 shr-r/m64-1
  sar-r/m32-cl sar-r/m64-cl
  shl-r/m32-cl shl-r/m64-cl
  shr-r/m32-cl shr-r/m64-cl
  sar-r/m32-imm8 sar-r/m64-imm8
  shl-r/m32-imm8 shl-r/m64-imm8
  shr-r/m32-imm8 shr-r/m64-imm8)


(define (interpret-shift cpu dst count proc)
  (define n (core:bv-size count))
  (define mask
    (if (equal? n 64) (bv #x3f n) (bv #x1f n)))
  (define v1 (cpu-gpr-ref cpu dst))
  (define v2 (bvand count mask))
  (define result (proc v1 v2))
  (cpu-gpr-set! cpu dst result)
  ; flags are unchanged for zero count (v2)
  (unless (bvzero? v2)
    (cpu-pf+zf+sf-set! cpu result)
    ; CF
    (cpu-flag-set! cpu 'CF
      (cond
        [(equal? proc bvshl)
         (msb (proc v1 (bvsub1 v2)))]
        [else
         (lsb (proc v1 (bvsub1 v2)))]))
    ; OF
    (cond
      [(equal? v2 (bv 1 n))
       (cond
         [(equal? proc bvshl)
          (cpu-flag-set! cpu 'OF (bvxor (msb result) (cpu-flag-ref cpu 'CF)))]
         [(equal? proc bvlshr)
          (cpu-flag-set! cpu 'OF (msb v1))]
         [(equal? proc bvashr)
          (cpu-flag-clear! cpu 'OF)])]
      [else ; not 0 or 1
       (cpu-flag-havoc! cpu 'OF)])
    ; AF
    (cpu-flag-havoc! cpu 'AF)))


(define-syntax (define-shift-1 stx)
  (syntax-case stx ()
    [(_ id opcode /n bvop)
     (with-syntax ([id-r/m32 (format-id stx "~a-r/m32-1" (syntax-e #'id))]
                   [id-r/m64 (format-id stx "~a-r/m64-1" (syntax-e #'id))])
       #'(begin
           (define-insn id-r/m32 (dst)
             #:decode [((byte opcode) (/n r/m))
                       (list (gpr32-no-rex r/m))]
                      [((rex/r b) (byte opcode) (/n r/m))
                       (list (gpr32 b r/m))]
             #:encode (list (rex/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (interpret-shift cpu dst (bv 1 32) bvop)))
           (define-insn id-r/m64 (dst)
             #:decode [((rex.w/r b) (byte opcode) (/n r/m))
                       (list (gpr64 b r/m))]
             #:encode (list (rex.w/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (interpret-shift cpu dst (bv 1 64) bvop)))))]))

; D1 /7
; REX.W D1 /7
(define-shift-1 sar #xD1 /7 bvashr)

; D1 /4
; REX.W D1 /4
(define-shift-1 shl #xD1 /4 bvshl)

; D1 /5
; REX.W D1 /5
(define-shift-1 shr #xD1 /5 bvlshr)


(define-syntax (define-shift-cl stx)
  (syntax-case stx ()
    [(_ id opcode /n bvop)
     (with-syntax ([id-r/m32 (format-id stx "~a-r/m32-cl" (syntax-e #'id))]
                   [id-r/m64 (format-id stx "~a-r/m64-cl" (syntax-e #'id))])
       #'(begin
           (define-insn id-r/m32 (dst)
             #:decode [((byte opcode) (/n r/m))
                       (list (gpr32-no-rex r/m))]
                      [((rex/r b) (byte opcode) (/n r/m))
                       (list (gpr32 b r/m))]
             #:encode (list (rex/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (interpret-shift cpu dst (zero-extend (cpu-gpr-ref cpu cl) (bitvector 32)) bvop)))
           (define-insn id-r/m64 (dst)
             #:decode [((rex.w/r b) (byte opcode) (/n r/m))
                       (list (gpr64 b r/m))]
             #:encode (list (rex.w/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (interpret-shift cpu dst (zero-extend (cpu-gpr-ref cpu cl) (bitvector 64)) bvop)))))]))

; D3 /7
; REX.W + D3 /7
(define-shift-cl sar #xD3 /7 bvashr)

; D3 /4
; REX.W D3 /4
(define-shift-cl shl #xD3 /4 bvshl)

; D3 /5
; REX.W D3 /5
(define-shift-cl shr #xD3 /5 bvlshr)


(define-syntax (define-shift-imm8 stx)
  (syntax-case stx ()
    [(_ id opcode /n bvop)
     (with-syntax ([id-r/m32 (format-id stx "~a-r/m32-imm8" (syntax-e #'id))]
                   [id-r/m64 (format-id stx "~a-r/m64-imm8" (syntax-e #'id))])
       #'(begin
           (define-insn id-r/m32 (dst imm8)
             #:decode [((byte opcode) (/n r/m) i0)
                       (list (gpr32-no-rex r/m) (decode-imm i0))]
                      [((rex/r b) (byte opcode) (/n r/m) i0)
                       (list (gpr32 b r/m) (decode-imm i0))]
             #:encode (list (rex/r dst) (byte opcode) (/n dst) (encode-imm imm8))
             (lambda (cpu dst imm8)
               (interpret-shift cpu dst (zero-extend imm8 (bitvector 32)) bvop)))
           (define-insn id-r/m64 (dst imm8)
             #:decode [((rex.w/r b) (byte opcode) (/n r/m) i0)
                       (list (gpr64 b r/m) (decode-imm i0))]
             #:encode (list (rex.w/r dst) (byte opcode) (/n dst) (encode-imm imm8))
             (lambda (cpu dst imm8)
               (interpret-shift cpu dst (zero-extend imm8 (bitvector 64)) bvop)))))]))


; C1 /7 ib
; REX.W C1 /7 ib
(define-shift-imm8 sar #xC1 /7 bvashr)

; C1 /4 ib
; REX.W C1 /4 ib
(define-shift-imm8 shl #xC1 /4 bvshl)

; C1 /5 ib
; REX.W C1 /5 ib
(define-shift-imm8 shr #xC1 /5 bvlshr)
