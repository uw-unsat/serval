#lang rosette

(require
  "common.rkt")

(provide
  div-r/m32
  div-r/m64)


(define (interpret-div cpu src n)
  (define lower (trunc n (cpu-gpr-ref cpu rax)))
  (define upper (trunc n (cpu-gpr-ref cpu rdx)))
  (define v (trunc n (cpu-gpr-ref cpu src)))
  (core:bug-on (bvzero? v)
    #:msg "divide by zero")
  (define result
    (cond
      [(bvzero? upper)
       (cons ((core:bvudiv-proc) lower v)
             ((core:bvurem-proc) lower v))]
      [else
       (define v1 (concat upper lower))
       (define v2 (concat (bv 0 n) v))
       (define q ((core:bvudiv-proc) v1 v2))
       (define r ((core:bvurem-proc) v1 v2))
       (core:bug-on (bvugt q (concat (bv 0 n) (bv -1 n)))
         #:msg "divide overflow")
       (cons (trunc n q)
             (trunc n r))]))
  (cpu-gpr-set! cpu rax (zero-extend (car result) (bitvector 64)))
  (cpu-gpr-set! cpu rdx (zero-extend (cdr result) (bitvector 64)))
  ; CF, OF, SF, ZF, AF, and PF are undefined
  (cpu-flag-havoc! cpu 'CF 'OF 'SF 'ZF 'AF 'PF))

; F7 /6
(define-insn div-r/m32 (src)
  #:decode [((byte #xF7) (/6 r/m))
            (list (gpr32-no-rex r/m))]
           [((rex/r b) (byte #xF7) (/6 r/m))
            (list (gpr32 b r/m))]
  #:encode (list (rex/r src) (byte #xF7) (/6 src))
  (lambda (cpu src)
    (interpret-div cpu src 32)))

; REX.W + F7 /6
(define-insn div-r/m64 (src)
  #:decode [((rex.w/r b) (byte #xF7) (/6 r/m))
            (list (gpr64 b r/m))]
  #:encode (list (rex.w/r src) (byte #xF7) (/6 src))
  (lambda (cpu src)
    (interpret-div cpu src 64)))
