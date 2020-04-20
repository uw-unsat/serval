#lang rosette

(require
  "common.rkt")

(provide
  shrd-r/m32-r32-imm8
  shrd-r/m64-r64-imm8
  shrd-r/m32-r32-cl
  shrd-r/m64-r64-cl)


(define (interpret-shrd cpu dst src count)
  (define size (core:bv-size count))
  (set! count (bvurem count (if (= size 64) (bv 64 size) (bv 32 size))))

  (define v1 (cpu-gpr-ref cpu dst))
  (define v2 (cpu-gpr-ref cpu src))

  ; The Intel manual says that the result is undefined if count > operand size;
  ; let's simply disallow such cases.
  (core:bug-on (! (equal? size (core:bv-size v1)))
   #:msg (format "dst must be of size ~a" size))
  (core:bug-on (! (equal? size (core:bv-size v2)))
   #:msg (format "src must be of size ~a" size))

  ; The Intel manual suggests "no operation" if count is 0.
  ; This is not accurate: the upper 32 bits of dst will be cleared.
  (define result (bvor (bvlshr v1 count)
                       (bvshl v2 (bvsub (bv size size) count))))
  (cpu-gpr-set! cpu dst result)

  (unless (bvzero? count)
    (cpu-pf+zf+sf-set! cpu result)
    ; CF is the last bit shifted out of dst.
    (cpu-flag-set! cpu 'CF (lsb (bvlshr v1 (bvsub1 count))))
    ; OF is set if there is a sign change for a 1-bit shift;
    ; it's undefined for shifts greater than 1.
    (cond
      [(bveq count (bv 1 size))
       (define sign-change? (! (bveq (msb v1) (lsb v2))))
       (cpu-flag-set! cpu 'OF (bool->bitvector sign-change?))]
      [else
       (cpu-flag-havoc! cpu 'OF)])
    ; AF is undefined.
    (cpu-flag-havoc! cpu 'AF)))


; 0F AC /r
(define-insn shrd-r/m32-r32-imm8 (dst src imm8)
  #:decode [((byte #x0F) (byte #xAC) (/r reg r/m) ib)
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg) ib)]
           [((rex/r r b) (byte #x0F) (byte #xAC) (/r reg r/m) ib)
            (list (gpr32 b r/m) (gpr32 r reg) ib)]
  #:encode (list (rex/r src dst) (byte #x0F) (byte #xAC) (/r src dst) imm8)
  (lambda (cpu dst src imm8)
    (interpret-shrd cpu dst src (zero-extend imm8 (bitvector 32)))))

; REX.W + 0F AC /r
(define-insn shrd-r/m64-r64-imm8 (dst src imm8)
  #:decode [((rex.w/r r b) (byte #x0F) (byte #xAC) (/r reg r/m) ib)
            (list (gpr64 b r/m) (gpr64 r reg) ib)]
  #:encode (list (rex.w/r src dst) (byte #x0F) (byte #xAC) (/r src dst) imm8)
  (lambda (cpu dst src imm8)
    (interpret-shrd cpu dst src (zero-extend imm8 (bitvector 64)))))

; 0F AD /r
(define-insn shrd-r/m32-r32-cl (dst src)
  #:decode [((byte #x0F) (byte #xAD) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x0F) (byte #xAD) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x0F) (byte #xAD) (/r src dst))
  (lambda (cpu dst src)
    (interpret-shrd cpu dst src (zero-extend (cpu-gpr-ref cpu cl) (bitvector 32)))))

; REX.W + 0F AD /r
(define-insn shrd-r/m64-r64-cl (dst src)
  #:decode [((rex.w/r r b) (byte #x0F) (byte #xAD) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x0F) (byte #xAD) (/r src dst))
  (lambda (cpu dst src)
    (interpret-shrd cpu dst src (zero-extend (cpu-gpr-ref cpu cl) (bitvector 64)))))
