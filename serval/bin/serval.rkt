#lang rosette

(require racket/logging
         (prefix-in core: serval/lib/core)
         (prefix-in llvm: serval/llvm))


; command-line options

(define option/entry (make-parameter #f))
(define option/keep (make-parameter #f))
(define option/level (make-parameter 'info))
(define option/trace (make-parameter #f))

(define (parse-command-line-options)
  (command-line
   #:once-each
   ["--function" name
                 "Set entry function"
                 (option/entry name)]
   ["--keep" "Keep temporary files"
             (option/keep #t)]
   ["--level" level
              "Set the log level: none|info|debug"
              (option/level (string->symbol level))]
   ["--trace" "Enable counterexample trace"
              (option/trace #t)]
   #:args (filename)
   filename))


; output

; create a child logger of topic 'serval
(define-logger serval)

(define color-output (make-parameter #f))

(define color-red "\033[31;1m")
(define color-green "\033[32;1m")
(define color-reset "\033[0m")

(define (color-wrap #:colored [colored (color-output)] color s)
  (if colored
      (string-append color s color-reset)
      s))

(define (color-wrap-port port color s)
  (color-wrap #:colored (terminal-port? port) color s))


(define-syntax-rule (log-info s ...)
  (log-serval-info s ...))

(define-syntax-rule (log-debug s ...)
  (log-serval-debug s ...))

(define (succ s)
  (color-wrap color-green s))

(define (fail s)
  (color-wrap color-red s))

(define (err . args)
  (define prefix (color-wrap-port (current-error-port) color-red "Error: "))
  (raise-user-error (string-append prefix (string-join args ": "))))

(define-syntax-rule (with-output body ...)
  (let [(port (current-output-port))]
    (color-output (terminal-port? port))
    (with-logging-to-port port
      (lambda () body ...)
      #:logger serval-logger
      (option/level)
      'serval)))


; intermediate files

(define (path->temporary-file path)
  (path->string
   (make-temporary-file
     (string-append "tmp~a-" (path->string (file-name-from-path path))))))

(define (llvm->racket filename)
  (log-info "Converting LLVM IR to Racket")
  (define llvm-rosette "o.riscv64/racket/llvm-rosette/llvm-rosette")
  (unless llvm-rosette
    (err "llvm-rosette" "Failed to locate"))
  (define tmp (path->temporary-file (path-replace-extension filename #".rkt")))
  (when (option/keep)
    (log-info "  ~a" tmp))
  (define s
    (with-output-to-string
      (lambda ()
        (unless (system* llvm-rosette "--function" (option/entry) filename)
          (err llvm-rosette "Failed to launch")))))
  (call-with-output-file tmp
    (lambda (out) (write-string s out))
    #:exists 'truncate/replace)
  tmp)

(define (c->llvm filename)
  (log-info "Converting C/C++ to LLVM IR")
  (define clang (find-executable-path "clang"))
  (unless clang
    (err "clang" "Failed to locate"))
  (define tmp (path->temporary-file (path-replace-extension filename #".ll")))
  (when (option/keep)
    (log-info "  ~a" tmp))
  (unless (system* clang "-S" "-emit-llvm" "-fno-discard-value-names" "-fsanitize=undefined" "-g" "-O2" "-o" tmp filename)
    (err clang "Failed to launch"))
  tmp)


; main

(define (make-function-arg arg)
  ; TODO: raise an exception if type is not bitvector
  ; make sure the symbolic name is unique
  (define name (car arg))
  (define type (cdr arg))
  (define-symbolic* function-arg type)
  (constant (cons name (list function-arg)) type))

(define (print-bug lst sol)
  (for ([data lst])
    (log-info "  ~a" (core:bug-format data sol))))

(define (print-counterexample f syms sol)
  (for ([sym syms]
        [arg (llvm:function-args f)])
    (define v (evaluate sym sol))
    (log-info "  ~a: ~a"
              (car arg)
              (if (term? v) "*" v))))

(define (main filename)
  (unless (file-exists? filename)
    (err filename "No such file"))

  (let [(ext (path-get-extension filename))]
    (cond
      [(member ext (list #".rkt"))
       ; keep the input file unconditionally
       (option/keep #t)]
      [(member ext (list #".ll"))
       ; use --keep as specified for .rkt
       (set! filename (llvm->racket filename))]
       [(member ext (list #".c" #".cc" #".cpp"))
       ; use --keep as specified for both .ll and .rkt
       (let ([tmp (c->llvm filename)])
         (set! filename (llvm->racket tmp))
         (unless (option/keep)
           (delete-file tmp)))]
      [else
       (err filename "Unsupported input file type")]))

  (log-info "Performing symbolic evaluation")

  (define f
    (dynamic-require (string->path filename)
                     (string->symbol (string-append "@" (option/entry)))))

  (define args (map make-function-arg (llvm:function-args f)))

  (define asserted
    (with-asserts-only
      (parameterize ([llvm:current-machine (llvm:make-machine null null)])
        (apply f args))))

  ; delete the temporary file (unless input is .rkt or -keep is specified)
  (unless (option/keep)
    (delete-file filename))

  (log-info "Solving constraints with ~a" (current-solver))

  (define nr-asserts (length asserted))
  (define nr-bugs
    (for/sum ([expr asserted]
              [i (in-range 1 (add1 nr-asserts))])
      (log-debug "Solving ~a/~a" i nr-asserts)
      ; TODO: skip checking if there is no bug data
      (define data (core:bug-ref expr))
      (define sol (verify (assert expr)))
      (cond
        [(unsat? sol)
         (log-debug (succ "  PASS"))
         0]
        [else
         (log-debug (fail "  FAIL"))
         (when (option/trace)
           (log-info "Violation:")
           (print-bug data sol)
           (log-info "Counterexample:")
           (print-counterexample f args sol))
         1])))
  (if (zero? nr-bugs)
    (log-info (succ "VERIFICATION SUCCEEDED"))
    (log-info (fail "VERIFICATION FAILED"))))


(let [(filename (parse-command-line-options))]
  (with-output (main filename)))
