#lang rosette/safe

(require
  serval/lib/core
  serval/lib/unittest
  (only-in racket/base parameterize exn:fail?)
  (prefix-in llvm: serval/llvm)
  (prefix-in rust-test: "o/rust_test.globals.rkt")
  (prefix-in rust-test: "o/rust_test.map.rkt")
  (prefix-in rust-test: "o/rust_test.ll.rkt")
)

(define (assert-always c)
  (check-unsat? (solve (assert (not c))))
)

; Takes a optional second argument which received the counter-example
(define (assert-not-always c [cex-sink void])
  (define cex (solve (assert (not c))))
  (check-sat? cex)
  (cex-sink cex)
)

(define (display-cex cex)
  (display (format "Counter example:\n~a\n" cex))
)



(define (check-add-u8)
  (parameterize ([llvm:current-machine (llvm:make-machine rust-test:symbols rust-test:globals)])

    ; manual tests
    ;(display (format "~a\n" (rust-test:@add_u8 (bv #x1 8) (bv #x2 8) (bv #x3 8))))
    ;(display (format "~a\n" (car (rust-test:@add_u8 (bv #x1 8) (bv #x2 8) (bv #x3 8)))))
    ;(display (format "~a\n" (cadr (rust-test:@add_u8 (bv #x1 8) (bv #x2 8) (bv #x3 8)))))

    ; symbolic inputs
    (define-symbolic a (bitvector 8))
    (define-symbolic b (bitvector 8))
    (define-symbolic c (bitvector 8))

    ; symbolic result
    (define r (rust-test:@add_u8 a b c))

    ; make sure result has the expected structure
    (assert-always (list? r))
    (assert-always (eq? (length r) 2))

    ; split result tuple
    (define sum (cadr r))
    (define carry (car r))
    (assert-always ((bitvector 8) sum))
    (assert-always ((bitvector 8) carry))

    ; make inputs and result be of the same type
    (define a16 (zero-extend a (bitvector 16)))
    (define b16 (zero-extend b (bitvector 16)))
    (define c16 (zero-extend c (bitvector 16)))
    (define r16 (concat (car r) (cadr r)))

    ; prove correctness
    (assert-always (eq? r16 (bvadd a16 b16 c16)))
))



(define (check-add-u8-buggy)
  (parameterize ([llvm:current-machine (llvm:make-machine rust-test:symbols rust-test:globals)])

    ; manual tests
    ;(display (format "~a\n" (rust-test:@add_u8_buggy (bv #x13 8) (bv #x37 8) (bv #x0 8))))

    ; symbolic inputs
    (define-symbolic a (bitvector 8))
    (define-symbolic b (bitvector 8))
    (define-symbolic c (bitvector 8))

    ; symbolic result
    (define r (rust-test:@add_u8_buggy a b c))

    ; make sure result has the expected structure
    (assert-always (list? r))
    (assert-always (eq? (length r) 2))

    ; split result tuple
    (define sum (cadr r))
    (define carry (car r))
    (assert-always ((bitvector 8) sum))
    (assert-always ((bitvector 8) carry))

    ; make inputs and result be of the same type
    (define a16 (zero-extend a (bitvector 16)))
    (define b16 (zero-extend b (bitvector 16)))
    (define c16 (zero-extend c (bitvector 16)))
    (define r16 (concat (car r) (cadr r)))

    ; prove that there is a bug
    (assert-not-always (eq? r16 (bvadd a16 b16 c16))
      ; Uncomment to see the generated counter-example
      ;display-cex
    )

    ; prove that the bug is a || b == 0x1337
    (define (assert-bug-is-1337 cex)
      (assert (eq? (cex a) (bv #x13 8)))
      (assert (eq? (cex b) (bv #x37 8)))
    )
    (assert-not-always (eq? r16 (bvadd a16 b16 c16)) assert-bug-is-1337)

    ; prove that these is only one bug
    (assert-always
      (implies
        (not (or (eq? a (bv #x13 8)) (eq? b (bv #x37 8))))
        (eq? r16 (bvadd a16 b16 c16))
      )
    )

))



(define (check-add-u32-in-u8)
  (parameterize ([llvm:current-machine (llvm:make-machine rust-test:symbols rust-test:globals)])

    ; manual tests
    ;(display (format "~a\n" (rust-test:@add_u32_in_u8 (bv #x1 32) (bv #x2 32))))

    ; symbolic inputs
    (define-symbolic a (bitvector 32))
    (define-symbolic b (bitvector 32))

    ; symbolic result
    (define r (rust-test:@add_u32_in_u8 a b))

    ; make sure result has the expected structure
    ; note: even though we return a tuple as with add_u8
    ;       the result in this case seems to be a 40 bit bitstring.
    (assert-always ((bitvector 40) r))

    (define sum (extract 39 8 r))
    (define carry (extract 7 0 r))
    (assert-always (or (eq? carry (bv 0 8)) (eq? carry (bv 1 8))))

    ; make inputs and result be of the same type
    (define a33 (zero-extend a (bitvector 33)))
    (define b33 (zero-extend b (bitvector 33)))
    (define r33 (concat (extract 0 0 carry) sum))

    ; prove correctness
    (assert-always (eq? r33 (bvadd a33 b33)))
))



(define rust-tests
  (test-suite+
   "Tests for rust-test"
    (test-case+ "add_u8 test" (check-add-u8))
    (test-case+ "add_u8_buggy test" (check-add-u8-buggy))
    (test-case+ "add_u32_in_u8 test" (check-add-u32-in-u8))
  ))

(module+ test
  (time (run-tests rust-tests)))
