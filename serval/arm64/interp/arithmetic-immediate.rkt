#lang rosette

(require
  "common.rkt")

(provide
  add-immediate adds-immediate
  sub-immediate subs-immediate)


(define (decode sf sh imm12 Rn Rd)
  (define d Rd)
  (define n Rn)
  (define datasize (if (bveq sf (bv #b1 1)) 64 32))

  (define imm
    (cond
      [(bveq sh (bv #b0 1))
       (zero-extend imm12 (bitvector datasize))]
      [(bveq sh (bv #b1 1))
       (zero-extend (concat imm12 (zeros 12)) (bitvector datasize))]))

  (values d n datasize imm))


(define (interpret-add-immediate cpu sf sh imm12 Rn Rd)
  (define-values (d n datasize imm) (decode sf sh imm12 Rn Rd))
  (define operand1 (trunc datasize (if (equal? n (integer->gpr 31)) (cpu-sp-ref cpu) (cpu-gpr-ref cpu n))))

  (define-values (result _)
    (add-with-carry operand1 imm (bv #b0 1)))

  (if (equal? d (integer->gpr 31))
      (cpu-sp-set! cpu result)
      (cpu-gpr-set! cpu d result)))


(define (interpret-adds-immediate cpu sf sh imm12 Rn Rd)
  (define-values (d n datasize imm) (decode sf sh imm12 Rn Rd))
  (define operand1 (trunc datasize (if (equal? n (integer->gpr 31)) (cpu-sp-ref cpu) (cpu-gpr-ref cpu n))))

  (define-values (result nzcv)
    (add-with-carry operand1 imm (bv #b0 1)))

  (cpu-nzcv-set! cpu nzcv)

  (cpu-gpr-set! cpu d result))


(define (interpret-sub-immediate cpu sf sh imm12 Rn Rd)
  (define-values (d n datasize imm) (decode sf sh imm12 Rn Rd))
  (define operand1 (trunc datasize (if (equal? n (integer->gpr 31)) (cpu-sp-ref cpu) (cpu-gpr-ref cpu n))))
  (define operand2 (bvnot imm))

  (define-values (result _)
    (add-with-carry operand1 operand2 (bv #b1 1)))

  (if (equal? d (integer->gpr 31))
      (cpu-sp-set! cpu result)
      (cpu-gpr-set! cpu d result)))


(define (interpret-subs-immediate cpu sf sh imm12 Rn Rd)
  (define-values (d n datasize imm) (decode sf sh imm12 Rn Rd))
  (define operand1 (trunc datasize (if (equal? n (integer->gpr 31)) (cpu-sp-ref cpu) (cpu-gpr-ref cpu n))))
  (define operand2 (bvnot imm))

  (define-values (result nzcv)
    (add-with-carry operand1 operand2 (bv #b1 1)))

  (cpu-nzcv-set! cpu nzcv)

  (cpu-gpr-set! cpu d result))


(define-insn (sf sh imm12 Rn Rd)
  #:encode (lambda (op S) (list sf (bv op 1) (bv S 1) (bv #b100010 6) sh imm12 Rn Rd))
  [(#b0 #b0) add-immediate  interpret-add-immediate]
  [(#b0 #b1) adds-immediate interpret-adds-immediate]
  [(#b1 #b0) sub-immediate  interpret-sub-immediate]
  [(#b1 #b1) subs-immediate interpret-subs-immediate])
