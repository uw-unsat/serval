#lang rosette/safe

(require
  serval/lib/core
  serval/lib/unittest
  (only-in racket/base parameterize exn:fail?)
  (prefix-in llvm: serval/llvm)
  (prefix-in riscv: serval/riscv/objdump)
  (prefix-in riscv: serval/riscv/shims)
  (prefix-in memset: "generated/racket/test/memset.globals.rkt")
  (prefix-in memset: "generated/racket/test/memset.asm.rkt")
  (prefix-in memset: "generated/racket/test/memset.map.rkt")
)

(require (prefix-in memset: "generated/racket/test/memset.ll.rkt"))

(define N (bvpointer 10))
(define-symbolic i j (bvpointer?))

(define (check-llvm-memset-a0)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (define asserted
      (with-asserts-only (memset:@test_a0)))
    ; partial eval should work for memset(a, 0, sizeof(struct A))
    (check-equal? asserted null)
    (define a (llvm:symbol->block 'a))
    ; fields in a[0] must be zero
    (check-equal? (mblock-iload a (list (bvpointer 0) 'x)) (bv 0 32))
    (check-equal? (mblock-iload a (list (bvpointer 0) 'b 'y)) (bv 0 32))
    (check-equal? (mblock-iload a (list (bvpointer 0) 'b 'z)) (bv 0 32))
    ; fields in a[1] don't have to be zero
    (check-sat? (solve (assert (not (bvzero? (mblock-iload a (list (bvpointer 1) 'x)))))))))

(define (check-llvm-memset-ai)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (define a (llvm:symbol->block 'a))
    (define old-j.x (mblock-iload a (list j 'x)))
    (define asserted
      (with-asserts-only (memset:@test_ai i)))

    ; no UB
    (check-unsat? (verify (assert (apply && asserted))))

    ; fields in a[i] must be zero
    (check-unsat? (verify (assert (=> (bvult i N) (bvzero? (mblock-iload a (list i 'x)))))))

    ; fields in a[j] (j <> i) are not changed
    (define new-j.x (mblock-iload a (list j 'x)))
    (check-unsat? (verify (assert (=> (not (equal? i j)) (equal? old-j.x new-j.x)))))))

(define (check-llvm-memset-an)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (define a (llvm:symbol->block 'a))
    (define asserted
      (with-asserts-only (memset:@test_an)))

    ; partial eval
    (check-equal? asserted null)

    ; fields in a[i] must be zero
    (check-unsat? (verify (assert (forall (list j) (=> (bvult j N) (bvzero? (mblock-iload a (list j 'x))))))))))

(define (check-llvm-memset-b)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (define a (llvm:symbol->block 'a))
    (define old-j.b.y (mblock-iload a (list j 'b 'y)))
    (define old-j.b.z (mblock-iload a (list j 'b 'z)))
    (define asserted
      (with-asserts-only (memset:@test_b i)))

    ; no UB
    (check-unsat? (verify (assert (apply && asserted))))

    ; fields in a[i].b must be zero
    (check-unsat? (verify (assert (=> (bvult i N) (bvzero? (mblock-iload a (list i 'b 'y)))))))
    (check-unsat? (verify (assert (=> (bvult i N) (bvzero? (mblock-iload a (list i 'b 'z)))))))

    ; fields in a[j] (j <> i) are not changed
    (define new-j.b.y (mblock-iload a (list j 'b 'y)))
    (define new-j.b.z (mblock-iload a (list j 'b 'z)))
    (check-unsat? (verify (assert (=> (not (equal? i j)) (equal? old-j.b.y new-j.b.y)))))
    (check-unsat? (verify (assert (=> (not (equal? i j)) (equal? old-j.b.z new-j.b.z)))))))

(define (check-llvm-memset-b-z)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (define a (llvm:symbol->block 'a))
    (define old-i.b.y (mblock-iload a (list i 'b 'y)))
    (define old-j.b.y (mblock-iload a (list j 'b 'y)))
    (define old-j.b.z (mblock-iload a (list j 'b 'z)))

    (define asserted
      (with-asserts-only (memset:@test_b_z i)))

    ; no UB
    (check-unsat? (verify (assert (apply && asserted))))

    ; a[i].b.z must be zero
    (check-unsat? (verify (assert (=> (bvult i N) (bvzero? (mblock-iload a (list i 'b 'z)))))))

    ; a[i].b.y must not be changed
    (check-unsat? (verify (assert (equal? old-i.b.y (mblock-iload a (list i 'b 'y))))))

    ; fields in a[j] (j <> i) are not changed
    (define new-j.b.y (mblock-iload a (list j 'b 'y)))
    (define new-j.b.z (mblock-iload a (list j 'b 'z)))
    (check-unsat? (verify (assert (=> (not (equal? i j)) (equal? old-j.b.y new-j.b.y)))))
    (check-unsat? (verify (assert (=> (not (equal? i j)) (equal? old-j.b.z new-j.b.z)))))))

(define (check-memset-pages)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (define pages (llvm:symbol->block 'pages))
    (define asserted
      (with-asserts-only (memset:@test_pages i j)))

    ; no UB
    (check-unsat? (verify (assert (apply && asserted))))

    (check-unsat? (verify (assert
      (=> (&& (bvult i j) (bvule j N))
          (equal? (mblock-iload pages (list i (bv 0 64))) (bv 0 8))))))

    (define-symbolic pindex poffset (bitvector 64))
    (check-unsat? (verify (assert
      (forall (list pindex poffset)
        (=> (&& (bvuge pindex i) (bvult pindex j) (bvule j N) (bvult poffset (bv 4096 64)))
            (equal? (mblock-iload pages (list pindex poffset)) (bv 0 8)))))))
  ))

(define (check-memset-buggy-too-large-a)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (with-asserts-only (memset:@test_buggy_too_large_a))))

(define (check-llvm-memset-buggy-too-large-a)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (with-asserts-only (memset:@test_buggy_too_large_a))))

(define (check-llvm-memset-buggy-too-large-b)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (with-asserts-only (memset:@test_buggy_too_large_b))))

(define (check-llvm-memset-buggy-out-of-bounds)
  (parameterize ([llvm:current-machine (llvm:make-machine memset:symbols memset:globals)])
    (define asserted (with-asserts-only (memset:@test_buggy_out_of_bounds i)))
    (check-sat? (verify (assert (apply && asserted))))))

(define (init-riscv-test-cpu testname #:shim [shim #t])
  (define cpu (riscv:init-cpu memset:symbols memset:globals))
  (define memset-symbol (find-symbol-by-name memset:symbols 'memset))
  (define mret-symbol (find-symbol-by-name memset:symbols 'mret))
  (define test-symbol (find-symbol-by-name memset:symbols testname))

  (when shim
    ; Enable the memset shim
    (riscv:cpu-add-shim! cpu (bv (car memset-symbol) 64) riscv:memset-shim))

  ; Set PC to test
  (riscv:set-cpu-pc! cpu (bv (car test-symbol) 64))
  ; Set return address to mret to regain control
  (riscv:gpr-set! cpu 'ra (bv (car mret-symbol) 64))
  (check-equal? (asserts) null)
  cpu)

(define (check-riscv-memset-a0)
  (define cpu (init-riscv-test-cpu 'test_a0))
  (define a (mregion-block (find-mregion-by-name (riscv:cpu-mregions cpu) 'a)))
  (riscv:interpret-objdump-program cpu memset:instructions)
  (check-equal? (mblock-iload a (list (bvpointer 0) 'x)) (bv 0 32))
  (check-equal? (mblock-iload a (list (bvpointer 0) 'b 'y)) (bv 0 32))
  (check-equal? (mblock-iload a (list (bvpointer 0) 'b 'z)) (bv 0 32))
  (void))

(define (check-riscv-memset-ai)
  (define cpu (init-riscv-test-cpu 'test_ai))
  (riscv:gpr-set! cpu 'a0 i)
  (define a (mregion-block (find-mregion-by-name (riscv:cpu-mregions cpu) 'a)))
  (define old-j.x (mblock-iload a (list j 'x)))

  (define asserted
    (with-asserts-only (riscv:interpret-objdump-program cpu memset:instructions)))
  (check-unsat? (verify (assert (apply && asserted))))

  (check-unsat? (verify (assert (=> (bvult i N) (bvzero? (mblock-iload a (list i 'x)))))))
  (define new-j.x (mblock-iload a (list j 'x)))
  (check-unsat? (verify (assert (=> (not (equal? i j)) (equal? old-j.x new-j.x))))))


(define (check-riscv-memset-shim-correct)
  (define shim-cpu (init-riscv-test-cpu 'test_byte_buffer))
  (define real-cpu (init-riscv-test-cpu 'test_byte_buffer #:shim #f))
  (define shim-buffer (mregion-block (find-mregion-by-name (riscv:cpu-mregions shim-cpu) 'buffer)))
  (define real-buffer (mregion-block (find-mregion-by-name (riscv:cpu-mregions real-cpu) 'buffer)))

  (define shim-asserted
    (with-asserts-only (riscv:interpret-objdump-program shim-cpu memset:instructions)))
  (check-equal? shim-asserted null)

  (define real-asserted
    (with-asserts-only (riscv:interpret-objdump-program real-cpu memset:instructions)))
  (check-equal? real-asserted null)

  (check-unsat? (verify (assert (forall (list i)
    (=>
      (bvult i N)
      (equal? (mblock-iload shim-buffer (list i))
              (mblock-iload real-buffer (list i))))))))

  (check-equal? (riscv:gpr-ref real-cpu 'a0) (riscv:gpr-ref shim-cpu 'a0))
)

(define memset-tests
  (test-suite+
   "Tests for memset"
    (test-case+ "llvm-memset-a0" (check-llvm-memset-a0))
    (test-case+ "llvm-memset-ai" (check-llvm-memset-ai))
    (test-case+ "llvm-memset-an" (check-llvm-memset-an))
    (test-case+ "llvm-memset-b" (check-llvm-memset-b))
    (test-case+ "llvm-memset-pages" (check-memset-pages))
    (test-case+ "llvm-memset-b-z" (check-llvm-memset-b-z))
    (test-case+ "llvm-memset-buggy-too-large-a" (check-exn exn:fail? check-llvm-memset-buggy-too-large-a))
    (test-case+ "llvm-memset-buggy-too-large-b" (check-exn exn:fail? check-llvm-memset-buggy-too-large-b))
    (test-case+ "llvm-memset-buggy-out-of-bounds" (check-llvm-memset-buggy-out-of-bounds))

    (test-case+ "riscv-memset-a0" (check-riscv-memset-a0))
    (test-case+ "riscv-memset-ai" (check-riscv-memset-ai))
    (test-case+ "riscv-memset-shim-correct" (check-riscv-memset-shim-correct))
  ))

(module+ test
  (time (run-tests memset-tests)))
