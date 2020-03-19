#lang rosette

(require
  serval/bpf
  serval/lib/unittest)

(provide (all-defined-out) (all-from-out serval/bpf serval/lib/unittest))

(define R0 BPF_REG_0)
(define R1 BPF_REG_1)
(define R2 BPF_REG_2)
(define R3 BPF_REG_3)
(define R4 BPF_REG_4)
(define R5 BPF_REG_5)
(define R6 BPF_REG_6)
(define R7 BPF_REG_7)
(define R8 BPF_REG_8)
(define R9 BPF_REG_9)
(define R10 BPF_REG_10)

(define (cpu_to_be16 x)
  (integer-bytes->integer (integer->integer-bytes x 2 #f #t) #f))

(define (cpu_to_be32 x)
  (integer-bytes->integer (integer->integer-bytes x 4 #f #t) #f))

(define (cpu_to_be64 x)
  (integer-bytes->integer (integer->integer-bytes x 8 #f #t) #f))

(define (cpu_to_le16 x)
  (integer-bytes->integer (integer->integer-bytes x 2 #f #f) #f))

(define (cpu_to_le32 x)
  (integer-bytes->integer (integer->integer-bytes x 4 #f #f) #f))

(define (cpu_to_le64 x)
  (integer-bytes->integer (integer->integer-bytes x 8 #f #f) #f))

(define (run-insns result . insns)
  (define cpu (init-cpu #f))
  (define actual (interpret-program cpu (make-insns insns)))
  (check-unsat? (verify (assert (equal? actual (bv result 32))))))

(define-syntax-rule (bpf-test-case name #:result result ...)
  (test-case+ name (run-insns result ...)))
