#lang rosette

(require
  "common.rkt")

(provide
  xchg-r/m32-r32
  xchg-r/m64-r64)


(define (interpret-xchg cpu dst src)
  (define mm (cpu-memmgr cpu))
  (when mm
    (core:memmgr-atomic-begin mm))
  (define temp (cpu-gpr-ref cpu dst))
  (cpu-gpr-set! cpu dst (cpu-gpr-ref cpu src))
  (cpu-gpr-set! cpu src temp)
  (when mm
    (core:memmgr-atomic-end mm)))

; 87 /r
(define-insn xchg-r/m32-r32 (dst src)
  #:decode [((byte #x87) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x87) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x87) (/r src dst))
  interpret-xchg)

(define-insn xchg-m32-r32 (dst src)
  #:decode [((byte #x87) (modr/m (== (bv #b00 2)) reg r/m))
            (list (register-indirect (gpr64-no-rex r/m) #f 32) (gpr32-no-rex reg))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex/r (car es) (first ed)) (byte #x87) (modr/m (second ed) (cdr es) (third ed)) (fourth ed)))
  interpret-xchg)

; REX.W + 87 /r
(define-insn xchg-r/m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x87) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x87) (/r src dst))
  interpret-xchg)
