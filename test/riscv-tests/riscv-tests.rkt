#lang rosette

(require
  serval/lib/unittest
  (prefix-in core: serval/lib/core)
  serval/riscv/interp
  serval/riscv/objdump
  serval/riscv/base)

(define (run-test testfile)
  ; (define globalsfile (string->path (string-replace (path->string testfile) ".asm.rkt" ".globals.rkt")))
  ; (define mapfile (string->path (string-replace (path->string testfile) ".asm.rkt" ".map.rkt")))
  (define instrs (dynamic-require testfile 'instructions))
  (define arch (dynamic-require testfile 'architecture))
  ; (define symbols (dynamic-require mapfile 'symbols))
  ; (define globals (dynamic-require globalsfile 'globals))

  (define bits (case arch [(riscv:rv64) 64] [(riscv:rv32) 32]))

  (parameterize
    ([XLEN bits]
     [core:target-pointer-bitwidth bits])

    (define cpu (init-cpu null null (lambda a (core:make-flat-memmgr))))

    (define (handle-failure e)
      (displayln e)
      (define s (format "FAILURE --> Test ~e failed in ~e\n" (bitvector->integer (gpr-ref cpu 'x21)) testfile))
      (assert #f s))

    (with-handlers
      ([exn:fail? handle-failure])
      (interpret-objdump-program cpu instrs))

    (printf "SUCCESS --> ~e\n" testfile)))

(define riscv-tests
  (test-suite+
   "Add test"
     (for ([filename (current-command-line-arguments)])
       (displayln filename)
       (test-case+ filename (run-test (build-path "../" filename))))
  ))

(module+ test
  (time (run-tests riscv-tests)))
