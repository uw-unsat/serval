#lang rosette

(require
  "common.rkt")

(provide
  rev
  rev16
  rev32
  rev64)


(define (decode-rev* sf opc Rn Rd)
  (define d Rd)
  (define n Rn)

  (define datasize (if (bveq sf (bv #b1 1)) 64 32))

  (define container_size
    (cond
      [(bveq opc (bv #b00 2))
       (unreachable)]
      [(bveq opc (bv #b01 2))
       16]
      [(bveq opc (bv #b10 2))
       32]
      [(bveq opc (bv #b11 2))
       (when (bveq sf (bv #b0 1))
         (undefined))
       64]))

  (values d n datasize container_size))


; It's okay to use integers here as they are concrete.
(define (interpret-rev* cpu sf opc Rn Rd)
  (define-values (d n datasize container_size) (decode-rev* sf opc Rn Rd))
  (define operand (trunc datasize (cpu-gpr-ref cpu n)))
  ; Use a vector of (bitvector 8) to hold the result.
  (define result (make-vector (quotient datasize 8) #f))

  (define containers (quotient datasize container_size))
  (define elements_per_container (quotient container_size 8))
  (define index 0)

  (for ([c (range containers)])
    (define rev_index (+ index (* (sub1 elements_per_container) 8)))
    (for ([e (range elements_per_container)])
      (vector-set! result (quotient rev_index 8) (extract (+ index 7) index operand))
      (set! index (+ index 8))
      (set! rev_index (- rev_index 8))))

  ; Need to reverse for concat.
  (cpu-gpr-set! cpu d (apply concat (reverse (vector->list result)))))


(define-insn (sf opc Rn Rd)
  #:encode (lambda () (list sf (bv #b1 1) (bv #b0 1) (bv #b11010110 8) (bv #b00000 5) (bv #b0000 4) opc Rn Rd))
  [() rev*  interpret-rev*])

(define (rev sf Rn Rd)
  (rev* sf (concat (bv #b1 1) sf) Rn Rd))

(define (rev16 sf Rn Rd)
  (rev* sf (bv #b01 2) Rn Rd))

(define (rev32 Rn Rd)
  (rev* (bv #b1 1) (bv #b10 2) Rn Rd))

; alias of rev
(define (rev64 Rn Rd)
  (rev (bv #b1 1) Rn Rd))
