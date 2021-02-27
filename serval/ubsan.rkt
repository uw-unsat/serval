#lang rosette

(require (prefix-in core: "lib/core.rkt"))

(provide (all-defined-out))

; types

(struct source-location (filename line column) #:transparent
 #:methods gen:custom-write
 [(define (write-proc loc port mode)
    (fprintf port "~a:~a" (source-location-filename loc) (source-location-line loc))
    (let ([column (source-location-column loc)])
      (when column
        (fprintf port ":~a" column))))])

(struct type-descriptor (kind info name) #:transparent)

(struct overflow-data (location type) #:transparent)

(struct out-of-bounds-data (location array-type index-type) #:transparent)

(struct shift-out-of-bounds-data (location lhs-type rhs-type) #:transparent)

; constructors

(define (make-overflow-data lst)
  (apply overflow-data
         (map apply (list source-location type-descriptor) lst)))

(define (make-out-of-bounds-data lst)
  (apply out-of-bounds-data
         (map apply (list source-location type-descriptor type-descriptor) lst)))

(define (make-shift-out-of-bounds-data lst)
  (apply shift-out-of-bounds-data
         (map apply (list source-location type-descriptor type-descriptor) lst)))

; helpers

(define (type-bit-width type)
  (arithmetic-shift 1 (arithmetic-shift (type-descriptor-info type) (- 1))))

(define (get-signed-val type val)
  (define n (type-bit-width type))
  (bitvector->integer (extract (sub1 n) 0 val)))

(define (get-unsigned-val type val)
  (bitvector->natural val))

(define (type-is-signed? type)
  (not (zero? (bitwise-and (type-descriptor-info type) 1))))

(define (val-is-negative? type val)
  (and (type-is-signed? type) (negative? (get-signed-val type val))))

(define (val->string type val)
  (if (type-is-signed? type)
      (get-signed-val type val)
      (get-unsigned-val type val)))

; handlers

; define a function that evaluates variables given a solution
(define-syntax (define-evaluate stx)
  (syntax-case stx ()
    [(_ (f x ...) body ...)
     (syntax/loc stx
       (define (f sol)
         (let ([x (evaluate x sol)]
               ...)
           body ...)))]))

(define (handle-integer-overflow lst lhs rhs op)
  (define data (make-overflow-data lst))
  (define type (overflow-data-type data))
  (define-evaluate (msg lhs rhs)
    (define lhs-str (val->string type lhs))
    (define rhs-str (val->string type rhs))
    (format "~a integer overflow: ~a ~a ~a cannot be represented in type ~a"
            (if (type-is-signed? type) "signed" "unsigned")
            lhs-str op rhs-str
            (type-descriptor-name type)))
  (core:bug #:msg (lambda (sol) (format "~v ~v ~v" "ubsan" (overflow-data-location data) (msg sol)))))

(define __ubsan_handle_add_overflow (curryr handle-integer-overflow "+"))
(define __ubsan_handle_sub_overflow (curryr handle-integer-overflow "-"))
(define __ubsan_handle_mul_overflow (curryr handle-integer-overflow "*"))

(define (__ubsan_handle_divrem_overflow lst lhs rhs)
  (define data (make-overflow-data lst))
  (define type (overflow-data-type data))
  (define-evaluate (msg lhs rhs)
    (cond
      [(bvzero? rhs)
       "division by zero"]
      [else
       (format "division of ~a by -1 cannot be represented in type ~a"
               (bitvector->integer lhs)
               (type-descriptor-name type))]))
  (core:bug #:msg (lambda (sol) (format "~v ~v ~v" "ubsan" (overflow-data-location data) (msg sol)))))

(define (__ubsan_handle_out_of_bounds lst index)
  (define data (make-out-of-bounds-data lst))
  (define array-type (out-of-bounds-data-array-type data))
  (define index-type (out-of-bounds-data-index-type data))
  (define-evaluate (msg index)
    (format "index ~a is out of range for type ~a"
            (val->string index-type index)
            (type-descriptor-name array-type)))
  (core:bug #:msg (lambda (sol) (format "~v ~v ~v" "ubsan" (out-of-bounds-data-location data) (msg sol)))))

(define (__ubsan_handle_shift_out_of_bounds lst lhs rhs)
  (define data (make-shift-out-of-bounds-data lst))
  (define lhs-type (shift-out-of-bounds-data-lhs-type data))
  (define rhs-type (shift-out-of-bounds-data-rhs-type data))
  (define-evaluate (msg lhs rhs)
    (define lhs-str (val->string lhs-type lhs))
    (define rhs-str (val->string rhs-type rhs))
    (cond
      [(val-is-negative? rhs-type rhs)
       (format "shift exponent ~a is negative" rhs-str)]
      [(>= (get-unsigned-val rhs-type rhs) (type-bit-width lhs-type))
       (format "shift exponent ~a is too large for ~a-bit type ~a"
               rhs-str (type-bit-width lhs-type) (type-descriptor-name lhs-type))]
      [(val-is-negative? lhs-type lhs)
       (format "left shift of negative value ~a" lhs-str)]
      [else
       (format "left shift of ~a by ~a places cannot be represented in type ~a"
               lhs-str rhs-str (type-descriptor-name lhs-type))]))
  (core:bug #:msg (lambda (sol) (format "~v ~v ~v" "ubsan" (shift-out-of-bounds-data-location data) (msg sol)))))
