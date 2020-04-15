#lang rosette

(require
  "common.rkt")

(provide
  movn movz movk)


; Helpers for updating bits<pos+15:pos>.
;
; We don't want to convert pos to integers, thus explicitly unrolling cases.
; Another approach is to use bit masking, but the code would be harder to read.

(define (update/32 v pos imm16)
  (cond
    [(bveq pos (bv 0 6))
     (concat (extract 31 16 v) imm16)]
    [(bveq pos (bv 16 6))
     (concat imm16 (extract 15 0 v))]
    [else
     (assert #f)]))

(define (update/64 v pos imm16)
  (cond
    [(bveq pos (bv 0 6))
     (concat (extract 63 16 v) imm16)]
    [(bveq pos (bv 16 6))
     (concat (extract 63 32 v) imm16 (extract 15 0 v))]
    [(bveq pos (bv 32 6))
     (concat (extract 63 48 v) imm16 (extract 31 0 v))]
    [(bveq pos (bv 48 6))
     (concat imm16 (extract 47 0 v))]
    [else
     (assert #f)]))

(define (update datasize v pos imm16)
  (case datasize
    [(32) (update/32 v pos imm16)]
    [(64) (update/64 v pos imm16)]
    [else (assert #f)]))


(define (decode sf hw imm16 Rd)
  (define d Rd)
  (define datasize (if (bveq sf (bv #b1 1)) 64 32))

  ; sf == '0' && hw<1> == '1'
  (when (&& (bveq sf (bv #b0 1)) (bveq (bit 1 hw) (bv #b1 1)))
    (undefined))

  (define pos (concat hw (bv #b0000 4)))

  (values d datasize pos))


(define (interpret-movn cpu sf hw imm16 Rd)
  (define-values (d datasize pos) (decode sf hw imm16 Rd))
  (define result (zeros datasize))
  (set! result (update datasize result pos imm16))
  (set! result (bvnot result))
  (cpu-gpr-set! cpu d result))


(define (interpret-movz cpu sf hw imm16 Rd)
  (define-values (d datasize pos) (decode sf hw imm16 Rd))
  (define result (zeros datasize))
  (set! result (update datasize result pos imm16))
  (cpu-gpr-set! cpu d result))


(define (interpret-movk cpu sf hw imm16 Rd)
  (define-values (d datasize pos) (decode sf hw imm16 Rd))
  (define result (trunc datasize (cpu-gpr-ref cpu d)))
  (set! result (update datasize result pos imm16))
  (cpu-gpr-set! cpu d result))


(define-insn (sf hw imm16 Rd)
  #:encode (lambda (opc) (list sf (bv opc 2) (bv #b100101 6) hw imm16 Rd))
  [(#b00) movn interpret-movn]
  [(#b10) movz interpret-movz]
  [(#b11) movk interpret-movk])
