#lang rosette

(require serval/lib/unittest)

(provide (all-defined-out))

(define
  (check-local-respect
    #:state-init init-state
    #:state-copy state-copy
    #:invariants inv
    #:dom dom
    #:u u
    #:unwinding unwinding
    #:flowsto flowsto
    action
    spec
    [args null])

  (define s (init-state))
  (define old-s (state-copy s))

  (apply spec s args)

  (for*/all ([doma (dom action old-s) #:exhaustive]
             [u u #:exhaustive])
    (begin
      (define pre (&& (inv old-s)
                      (! (flowsto doma u))))

      (define post (check-vc (unwinding u old-s s)))

      (check-unsat? (verify (assert (=> pre post)))))))

(define
  (check-step-consistency
    #:state-init init-state
    #:state-copy state-copy
    #:unwinding unwinding
    spec
    [args null])
  (define s (init-state))
  (define t (init-state))
  (define old-s (state-copy s))
  (define old-t (state-copy t))

  (apply spec s args)
  (apply spec t args)

  (define pre (unwinding old-s old-t))
  (define post (unwinding s t))

  (check-unsat? (verify (assert (=> pre post)))))

(define
  (check-weak-step-consistency
    #:state-init init-state
    #:state-copy state-copy
    #:invariants inv
    #:dom dom
    #:u u
    #:unwinding unwinding
    #:flowsto flowsto
    action
    spec
    [args null])

  (define s (init-state))
  (define t (init-state))
  (define old-s (state-copy s))
  (define old-t (state-copy t))

  (apply spec s args)
  (apply spec t args)

  (for*/all ([u u #:exhaustive]
             [doma (dom action old-s) #:exhaustive])
    (begin

      (define pre (&& (inv old-s)
                      (inv old-t)
                      (unwinding u old-s old-t)
                      (flowsto doma u)
                      (unwinding doma old-s old-t)
      ))

      (define post (check-vc (unwinding u s t)))

      (check-unsat? (verify (assert (=> pre post)))))))
