#lang rosette

(require
  (prefix-in core: "../lib/core.rkt")
  (only-in "base.rkt" instruction-encode))

(provide
  add-decoder decode)


(define debug (make-parameter (getenv "DEBUG_X86")))
(define decoders null)

(define (add-decoder proc)
  (set! decoders (cons proc decoders)))

(define (decode lst)
  (cond
    [(null? lst) null]
    [else
     (define result (filter-map (lambda (proc) (proc lst)) decoders))
     ; JIT might produce infeasible bytes under symbolic evaluation.
     ; Use assert to prune such cases.
     (when (null? result)
       (when (debug)
         (disassemble lst))
       (core:bug #:msg (format "no decoder for ~a" lst)))
     (core:bug-on (! (= (length result) 1)) #:msg (format "ambiguity in decoding for ~a" lst))
     (define insn (car (first result)))
     (define rest (cdr (first result)))
     (define act-bytes (take lst (- (length lst) (length rest))))
     (define exp-bytes (instruction-encode insn))
     (assert (equal? act-bytes exp-bytes)
             "encoded bytes must match original bytes")
     (cons insn (decode rest))]))

(define (disassemble lst)
  (eprintf "no decoder for ~a\n" lst)
  (set! lst (evaluate lst (complete-solution (sat) (symbolics lst))))
  (define in (open-input-string (string-join (map (lambda (x) (format "0x~x " (bitvector->natural x))) lst))))
  (define out (open-output-string))
  (parameterize ([current-input-port in]
                 [current-output-port out])
    (system* (find-executable-path "llvm-mc") "--disassemble"))
  (displayln (get-output-string out))
  (exit 1))
