#lang rosette/safe

(require
  "../lib/core.rkt"
  "../lib/unittest.rkt"
)

(provide (all-defined-out))

(define
  (verify-refinement
    #:implstate impl-state
    #:impl impl-func
    #:specstate spec-state
    #:spec spec-func
    #:abs abs-function
    #:ri rep-invariant
    [args null]
    [ce-handler (lambda (sol) (void))])

  (define ri0 (rep-invariant impl-state))

  (define pre (check-vc (equal? spec-state (abs-function impl-state))))

  (assume ri0)
  (assume pre)

  ; spec state transition
  (apply spec-func spec-state args)

  ; impl state transition
  (apply impl-func impl-state args)

  (define ri1 (rep-invariant impl-state))
  (assert ri1)

  (define post (equal? spec-state (abs-function impl-state)))
  (assert post))
