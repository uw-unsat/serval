#lang rosette

(require serval/lib/core
         serval/llvm
         serval/lib/unittest)

(require "generated/racket/test/bext.ll.rkt")

; Racket implementation
(define (bext_rkt rs1 rs2)
  (define r (bv 0 32))
  (define j (bv 0 32))
  (for ([i (range 32)])
    (when (! (bveq (bv 0 32) (bvand (bv 1 32) (bvashr rs2 (bv i 32)))))
      (when (! (bveq (bv 0 32) (bvand (bv 1 32) (bvashr rs1 (bv i 32)))))
        (set! r (bvor r (bvshl (bv 1 32) j))))
      (set! j (bvadd (bv 1 32) j))))
  r)

(define (check-bext)
  (parameterize ([current-machine (make-machine null null)])
    (define-symbolic* rs1 rs2 (bitvector 32))
    (displayln "executing...")
    (define-values (val asserted) (with-asserts (@bext rs1 rs2)))
    (displayln "solving...")

    ; The generated terms are too complicated to prove things about directly,
    ; so just prove it's equal for one particular value of rs2 as a sanity check.
    (check-unsat? (verify (assert (=> (bveq rs2 (bv #xfffdfffe 32)) (bveq val (bext_rkt rs1 rs2))))))

    (define sol (verify (assert (apply && asserted))))
    (check-unsat? sol)))

(define bext-tests
  (test-suite+
   "Tests for bext merging"
    (test-case+ "bext check" (check-bext))
  ))

; (module+ test
;   (time (run-tests bext-tests)))
