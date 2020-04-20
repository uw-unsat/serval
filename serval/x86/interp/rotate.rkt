#lang rosette

(require
  "common.rkt")

(provide
  rol-r/m16-1 rol-r/m32-1 rol-r/m64-1
  ror-r/m16-1 ror-r/m32-1 ror-r/m64-1
  rol-r/m16-cl rol-r/m32-cl rol-r/m64-cl
  ror-r/m16-cl ror-r/m32-cl ror-r/m64-cl
  rol-r/m16-imm8 rol-r/m32-imm8 rol-r/m64-imm8
  ror-r/m16-imm8 ror-r/m32-imm8 ror-r/m64-imm8)


(define (interpret-rol cpu dst count)
  (define n (core:bv-size count))
  (define mask (if (equal? n 64) (bv #x3f n) (bv #x1f n)))
  (define v1 (trunc n (cpu-gpr-ref cpu dst)))
  (define v2 (bvand count mask))
  (define result (bvrol v1 v2))
  (cpu-gpr-set! cpu dst result)
  ; flags are unchanged for zero count
  (unless (bvzero? v2)
    ; SF, ZF, AF, and PF are always unaffected
    (cpu-flag-set! cpu 'CF (lsb result))
    (if (equal? v2 (bv 1 n))
        (cpu-flag-set! cpu 'OF (bvxor (msb result) (cpu-flag-ref cpu 'CF)))
        (cpu-flag-havoc! cpu 'OF))))

(define (interpret-ror cpu dst count)
  (define n (core:bv-size count))
  (define mask (if (equal? n 64) (bv #x3f n) (bv #x1f n)))
  (define v1 (trunc n (cpu-gpr-ref cpu dst)))
  (define v2 (bvand count mask))
  (define result (bvror v1 v2))
  (cpu-gpr-set! cpu dst result)
  ; flags are unchanged for zero count
  (unless (bvzero? v2)
    ; SF, ZF, AF, and PF are always unaffected
    (cpu-flag-set! cpu 'CF (msb result))
    (if (equal? v2 (bv 1 n))
        (cpu-flag-set! cpu 'OF (bvxor (msb result) (bit (- n 2) result)))
        (cpu-flag-havoc! cpu 'OF))))

(define-syntax (define-rotate-1 stx)
  (syntax-case stx ()
    [(_ id opcode /n)
     (with-syntax ([proc (format-id stx "interpret-~a" (syntax-e #'id))]
                   [id-r/m16 (format-id stx "~a-r/m16-1" (syntax-e #'id))]
                   [id-r/m32 (format-id stx "~a-r/m32-1" (syntax-e #'id))]
                   [id-r/m64 (format-id stx "~a-r/m64-1" (syntax-e #'id))])
       #'(begin
           (define-insn id-r/m16 (dst)
             #:decode [((byte #x66) (byte opcode) (/n r/m))
                       (list (gpr16-no-rex r/m))]
                      [((byte #x66) (rex/r b) (byte opcode) (/n r/m))
                       (list (gpr16 b r/m))]
             #:encode (list (byte #x66) (rex/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (proc cpu dst (bv 1 16))))
           (define-insn id-r/m32 (dst)
             #:decode [((byte opcode) (/n r/m))
                       (list (gpr32-no-rex r/m))]
                      [((rex/r b) (byte opcode) (/n r/m))
                       (list (gpr32 b r/m))]
             #:encode (list (rex/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (proc cpu dst (bv 1 32))))
           (define-insn id-r/m64 (dst)
             #:decode [((rex.w/r b) (byte opcode) (/n r/m))
                       (list (gpr64 b r/m))]
             #:encode (list (rex.w/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (proc cpu dst (bv 1 64))))))]))

; D1 /0
; REX.W D1 /0
(define-rotate-1 rol #xD1 /0)

; D1 /1
; REX.W D1 /1
(define-rotate-1 ror #xD1 /1)


(define-syntax (define-rotate-cl stx)
  (syntax-case stx ()
    [(_ id opcode /n)
     (with-syntax ([proc (format-id stx "interpret-~a" (syntax-e #'id))]
                   [id-r/m16 (format-id stx "~a-r/m16-cl" (syntax-e #'id))]
                   [id-r/m32 (format-id stx "~a-r/m32-cl" (syntax-e #'id))]
                   [id-r/m64 (format-id stx "~a-r/m64-cl" (syntax-e #'id))])
       #'(begin
           (define-insn id-r/m16 (dst)
             #:decode [((byte #x66) (byte opcode) (/n r/m))
                       (list (gpr16-no-rex r/m))]
                      [((byte #x66) (rex/r b) (byte opcode) (/n r/m))
                       (list (gpr16 b r/m))]
             #:encode (list (byte #x66) (rex/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (proc cpu dst (zero-extend (cpu-gpr-ref cpu cl) (bitvector 16)))))
           (define-insn id-r/m32 (dst)
             #:decode [((byte opcode) (/n r/m))
                       (list (gpr32-no-rex r/m))]
                      [((rex/r b) (byte opcode) (/n r/m))
                       (list (gpr32 b r/m))]
             #:encode (list (rex/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (proc cpu dst (zero-extend (cpu-gpr-ref cpu cl) (bitvector 32)))))
           (define-insn id-r/m64 (dst)
             #:decode [((rex.w/r b) (byte opcode) (/n r/m))
                       (list (gpr64 b r/m))]
             #:encode (list (rex.w/r dst) (byte opcode) (/n dst))
             (lambda (cpu dst)
               (proc cpu dst (zero-extend (cpu-gpr-ref cpu cl) (bitvector 64)))))))]))

; D3 /0
; REX.W + D3 /0
(define-rotate-cl rol #xD3 /0)

; D3 /1
; REX.W D3 /1
(define-rotate-cl ror #xD3 /1)


(define-syntax (define-rotate-imm8 stx)
  (syntax-case stx ()
    [(_ id opcode /n)
     (with-syntax ([proc (format-id stx "interpret-~a" (syntax-e #'id))]
                   [id-r/m16 (format-id stx "~a-r/m16-imm8" (syntax-e #'id))]
                   [id-r/m32 (format-id stx "~a-r/m32-imm8" (syntax-e #'id))]
                   [id-r/m64 (format-id stx "~a-r/m64-imm8" (syntax-e #'id))])
       #'(begin
           (define-insn id-r/m16 (dst imm8)
             #:decode [((byte #x66) (byte opcode) (/n r/m) i0)
                       (list (gpr16-no-rex r/m) (decode-imm i0))]
                      [((byte #x66) (rex/r b) (byte opcode) (/n r/m) i0)
                       (list (gpr16 b r/m) (decode-imm i0))]
             #:encode (list (byte #x66) (rex/r dst) (byte opcode) (/n dst) (encode-imm imm8))
             (lambda (cpu dst imm8)
               (proc cpu dst (zero-extend imm8 (bitvector 16)))))
           (define-insn id-r/m32 (dst imm8)
             #:decode [((byte opcode) (/n r/m) i0)
                       (list (gpr32-no-rex r/m) (decode-imm i0))]
                      [((rex/r b) (byte opcode) (/n r/m) i0)
                       (list (gpr32 b r/m) (decode-imm i0))]
             #:encode (list (rex/r dst) (byte opcode) (/n dst) (encode-imm imm8))
             (lambda (cpu dst imm8)
               (proc cpu dst (zero-extend imm8 (bitvector 32)))))
           (define-insn id-r/m64 (dst imm8)
             #:decode [((rex.w/r b) (byte opcode) (/n r/m) i0)
                       (list (gpr64 b r/m) (decode-imm i0))]
             #:encode (list (rex.w/r dst) (byte opcode) (/n dst) (encode-imm imm8))
             (lambda (cpu dst imm8)
               (proc cpu dst (zero-extend imm8 (bitvector 64)))))))]))


; C1 /0 ib
; REX.W C1 /0 ib
(define-rotate-imm8 rol #xC1 /0)

; C1 /1 ib
; REX.W C1 /1 ib
(define-rotate-imm8 ror #xC1 /1)
