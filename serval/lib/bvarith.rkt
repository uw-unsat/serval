#lang rosette

(require "debug.rkt")

(provide (all-defined-out))

(define (trunc n x)
  (extract (sub1 n) 0 x))

(define i8 (bitvector 8))
(define i16 (bitvector 16))
(define i32 (bitvector 32))
(define i64 (bitvector 64))

(define (bv8 x) (bv x 8))
(define (bv16 x) (bv x 16))
(define (bv32 x) (bv x 32))
(define (bv64 x) (bv x 64))

(define (make-arg type)
  (define-symbolic* symbolic-arg type)
  symbolic-arg)

(define (make-bv8)
  (make-arg (bitvector 8)))

(define (make-bv16)
  (make-arg (bitvector 16)))

(define (make-bv32)
  (make-arg (bitvector 32)))

(define (make-bv64)
  (make-arg (bitvector 64)))

(define (bv-size x)
  (bitvector-size (type-of x)))

; i can be a symbolic bitvector
(define (bv-bit i x)
  (define mask (bvshl (bv 1 (bv-size i)) i))
  (if (bvzero? (bvand x mask))
      (bv 0 1)
      (bv 1 1)))

(define bv-sign msb)

(define (bvaligned? value alignment)
  (bvzero? (bvurem value alignment)))

(define (bvsmax? x)
  (define (bvsmax t)
    (bv (sub1 (arithmetic-shift 1 (sub1 (bitvector-size t)))) t))
  (bveq x (bvsmax (type-of x))))

(define (bvsmin? x)
  (define (bvsmin t)
    (bv (- (arithmetic-shift 1 (sub1 (bitvector-size t)))) t))
  (bveq x (bvsmin (type-of x))))

; list of (bitvector 8) -> (bitvector N), big endian
(define (list->bitvector/be lst)
  (apply concat lst))

; list of (bitvector 8) -> (bitvector N), little endian
(define (list->bitvector/le lst)
  (list->bitvector/be (reverse lst)))

; (bitvector N) -> list of (bitvector 8), big endian
(define (bitvector->list/be x)
  (reverse (bitvector->list/le x)))

; (bitvector N) -> list of (bitvector 8), little endian
(define (bitvector->list/le x)
  (define n (bv-size x))
  (map (lambda (i) (extract (+ i 7) i x)) (range 0 n 8)))

; bitvector overflow detection, following the enoding in MSR-TR-2009-57:
;   Modular Bug-finding for Integer Overflows in the Large:
;   Sound, Efficient, Bit-precise Static Analysis

(define (bvsadd-overflow? x y [carry #f])
  (define n (bv-size x))
  (define result (bvadd x y (if carry (bv 1 n) (bv 0 n))))
  (&& (bveq (bv-sign x) (bv-sign y))
      (! (bveq (bv-sign x) (bv-sign result)))))

(define (bvuadd-overflow? x y [carry #f])
  (define n (bv-size x))
  (bveq (bv 1 1)
        (bv-sign (bvadd (zero-extend x (bitvector (add1 n)))
                        (zero-extend y (bitvector (add1 n)))
                        (if carry (bv 1 (add1 n)) (bv 0 (add1 n)))))))

(define (bvssub-overflow? x y [borrow #f])
  (define n (bv-size x))
  (define result (bvsub x y (if borrow (bv 1 n) (bv 0 n))))
  (&& (! (bveq (bv-sign x) (bv-sign y)))
      (! (bveq (bv-sign x) (bv-sign result)))))

(define (bvusub-overflow? x y [borrow #f])
  (if borrow (bvule x y) (bvult x y)))

; signed multiplication overflow:
; multiply x and y sign-extended to 2N bits and check whether
; the result fits in N bits.
(define (bvsmul-overflow? x y)
  (let [(n (bv-size x))]
    (! (bveq
      (if (bveq (bv-sign x) (bv-sign y))
          (bv 0 (add1 n))
          (bv -1 (add1 n)))
      (extract (sub1 (* n 2)) (sub1 n)
        (bvmul (sign-extend x (bitvector (* n 2)))
               (sign-extend y (bitvector (* n 2)))))))))

; unsigned multiplication overflow:
; multiply x and y zero-extended to 2N bits and check whether
; the result fits in N bits.
(define (bvumul-overflow? x y)
  (let [(n (bv-size x))]
    (! (bvzero?
      (extract (sub1 (* n 2)) n
        (bvmul (zero-extend x (bitvector (* n 2)))
               (zero-extend y (bitvector (* n 2)))))))))

(define (bvsdiv-overflow? x y)
  (&& (bvsmin? x) (bveq y (bv -1 (type-of y)))))

(define bvmul-proc (make-parameter bvmul))
(define bvsdiv-proc (make-parameter bvsdiv))
(define bvsrem-proc (make-parameter bvsrem))
(define bvudiv-proc (make-parameter bvudiv))
(define bvurem-proc (make-parameter bvurem))

(define bvmulh-proc
  (make-parameter
    (lambda (x y)
      (let* ([w (bv-size x)]
             [dw (+ w w)])
        (extract (sub1 dw) w ((bvmul-proc) (sign-extend x (bitvector dw)) (sign-extend y (bitvector dw))))))))

(define bvmulhu-proc
  (make-parameter
    (lambda (x y)
      (let* ([w (bv-size x)]
             [dw (+ w w)])
        (extract (sub1 dw) w ((bvmul-proc) (zero-extend x (bitvector dw)) (zero-extend y (bitvector dw))))))))

(define bvmulhsu-proc
  (make-parameter
    (lambda (x y)
      (let* ([w (bv-size x)]
             [dw (+ w w)])
        (extract (sub1 dw) w ((bvmul-proc) (sign-extend x (bitvector dw)) (zero-extend y (bitvector dw))))))))
