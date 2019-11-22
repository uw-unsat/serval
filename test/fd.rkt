#lang rosette/safe

(require
  (except-in rackunit fail)
  (only-in racket/base parameterize)
  serval/llvm
  serval/lib/core
  serval/lib/unittest
  "generated/racket/test/fd.globals.rkt"
  "generated/racket/test/fd.map.rkt"
)

(require "generated/racket/test/fd.ll.rkt")

(define NR_PROCS 64)
(define NR_FDS 16)
(define NR_FILES 128)

(define-symbolic current (bitvector 64))
(define-symbolic proc-fds (~> (bitvector 64) (bitvector 64) (bitvector 64)))
(define-symbolic file-refcount (~> (bitvector 64) (bitvector 64)))

(define (impl-inv)
  (bvult (mblock-iload (symbol->block 'current) null) (bv NR_PROCS 64)))

(define (state-eqv)
  (define-symbolic pid (bitvector 64))
  (define-symbolic fd (bitvector 64))
  (define-symbolic fileid (bitvector 64))
  (define block-current (symbol->block 'current))
  (define block-procs (symbol->block 'procs))
  (define block-files (symbol->block 'files))
  (&& (equal? current (mblock-iload block-current null))
      (forall (list pid fd) (=> (&& (bvult pid (bv NR_PROCS 64)) (bvult fd (bv NR_FDS 64)))
                                (equal? (proc-fds pid fd) (mblock-iload block-procs (list pid 'fds fd)))))
      (forall (list fileid) (=> (bvult fileid (bv NR_FILES 64))
                                (equal? (file-refcount fileid) (mblock-iload block-files (list fileid 'refcount)))))))

(define (lo-spec-close fd)
  (define fileid (proc-fds current fd))
  (if (&& (bvult fd (bv NR_FDS 64)) (bvult fileid (bv NR_FILES 64)))
      (begin
        (define tmp file-refcount)
        (set! file-refcount (lambda (x) (if (equal? x fileid) (bvadd (tmp x) (bv -1 64)) (tmp x))))
        (bv 0 32))
      (bv -1 32)))

(define (check-fd-ref)
  (define-symbolic fd (bitvector 64))
  (define pre-eqv (state-eqv))
  (define pre-inv (impl-inv))
  (define pre (&& pre-eqv pre-inv))
  (lo-spec-close fd)
  (define-values (post asserted)
    (with-asserts
        (begin
          (@close fd)
          (&& (state-eqv) (impl-inv)))))
  ; no UB triggered
  (check-equal? (asserts) null)
  (for-each (lambda (x) (check-unsat? (verify (assert (=> pre-inv x))))) asserted)
  ; refinement
  (check-unsat? (verify (assert (implies pre post)))))

(define fd-tests
  (test-suite+
   "Tests for fd.c"

   (test-case+ "fd-ref" (parameterize ([current-machine (make-machine symbols globals)])
     (check-fd-ref)))))

(module+ test
  (time (run-tests fd-tests)))
