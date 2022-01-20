#lang racket/base

(require racket/format
         racket/list
         racket/string
         (prefix-in core: serval/lib/core)
         "base.rkt")

(provide print-module)

(define (print-module m [out (current-output-port)] #:extra-requires [extra-requires null])
  (displayln (string-join (list "; DO NOT MODIFY."
                                ";"
                                "; This file was automatically generated."
                                ""
                                "#lang rosette"
                                ""
                                "(provide (all-defined-out))"
                                ""
                                "(require (prefix-in core: serval/lib/core)"
                                (string-join (map (lambda (r) (format "~s" r)) extra-requires)
                                             "\n         "
                                             #:before-first "         ")
                                "         serval/llvm"
                                "         serval/ubsan)"
                                "")
                          "\n")
             out)

  (define globals
    (for/list ([gv (module-globals m)])
      (global->string gv)))
  (define functions
    (for/list ([f (module-functions m)])
      (function->string f)))
  (displayln (string-join (list (string-join globals "\n") (string-join functions "\n\n")) "\n\n")
             out))

(define (global->string gv)
  (format "(define-global ~a)" gv))

(define (function->string f)
  (define args
    (for/list ([arg (function-arguments f)])
      (format " ~a" (value-name arg))))
  (define hd (format "(define (~a~a)\n" (value-name f) (apply string-append args)))
  (define blocks
    (for/list ([bb (function-blocks f)])
      (block->string bb)))

  (cond
    ; If there are no basic blocks, treat the definition as an "extern" declaration
    ; and emit no Racket code. NB: is there a better way of determining if a function is not
    ; defined from the C API?
    [(null? blocks) ""]
    [else
     (define vars
       (flatten (for/list ([bb (function-blocks f)])
                  (filter value-type (basic-block-instructions bb)))))
     (define prologue
       (append (list "\n")
               (for/list ([v vars])
                 (format "  (define-value ~a)" (value-name v)))
               (list (format "  (enter! ~a)" (value-name (first (function-blocks f)))))))
     (string-append hd (string-join blocks "\n\n") (string-join prologue "\n") ")")]))

(define (block->string bb)
  (define name (value-name bb))
  (define hd (format "; ~a\n  (define-label (~a) #:merge #f\n" name name))
  (define insns
    (for/list ([insn (basic-block-instructions bb)] #:unless (instruction-is-dbg? insn))
      (string-append "    " (instruction->string insn))))
  (string-append hd (string-join insns "\n") ")"))

(define (instruction-is-dbg? insn)
  (and (string=? (opcode->string (instruction-opcode insn)) "call")
       (string-prefix? (symbol->string (list-ref (instruction-operands insn) 0)) "@llvm.dbg.")))

(define (instruction->string insn)
  (define body
    (string-append "("
                   (opcode->string (instruction-opcode insn))
                   (apply string-append
                          (map (lambda (x) (string-append " " (operand->string x insn)))
                               (instruction-operands insn)))
                   (apply string-append
                          (map (lambda (x) (format " #:~a ~a" (car x) (cdr x)))
                               (instruction-attributes insn)))
                   ")"))
  (if (value-type insn) (format "(set! ~a ~a)" (value-name insn) body) body))

(define (opcode->string opcode)
  (if (list? opcode) (string-join (map ~a opcode) "/") (~a opcode)))

(define (type->string type)
  (cond
    [(list? type) (format "(list ~a~a)"
                          (type->string (car type))
                          (string-join (for/list ([subtype (cdr type)])
                                         (string-append " " (type->string subtype)))))]
    [else (format "~a" type)]))

(define (operand->string v insn)
  (define opcode (instruction-opcode insn))
  (cond
    [(and (or (equal? opcode 'phi) (equal? opcode 'switch)) (pair? v))
     (format "[~a ~a]" (car v) (cdr v))]
    [(list? v) (string-join (for/list ([op v])
                              (string-append " " (operand->string op insn)))
                            ""
                            #:before-first "(list"
                            #:after-last ")")]
    [(array-offset? v) (format "(array-offset ~a ~a)" (array-offset-index v) (array-offset-size v))]
    [(struct-offset? v) (format "(struct-offset ~a)" (struct-offset-value v))]
    [(nullptr? v) "nullptr"]
    [(undef? v) (format "(undef ~a)" (type->string (undef-type v)))]
    [(core:marray? v) (format "(core:marray ~a ~a)"
                              (core:marray-length v)
                              (operand->string (core:marray-elements v) insn))]
    [(core:mstruct? v)
     (format "(core:mstruct ~a (list~a))"
             (core:mstruct-size v)
             (string-join (for/list ([f (core:mstruct-fields v)])
                            (format " (core:mfield ~a ~a ~a)"
                                    (core:mfield-name f)
                                    (core:mfield-offset f)
                                    (operand->string (core:mfield-element f) insn)))
                          ""))]
    [(core:mcell? v) (format "(core:mcell ~a)" (core:mcell-size v))]
    [(asm? v) (let ([template (asm-template v)])
                (cond
                  [(equal? template "") "'nop"]
                  [(not (string-contains? template " ")) (string-append "'" template)]
                  [else (~s template)]))]
    ; constant expr
    [(instruction? v) (instruction->string v)]
    [else (~s v)]))
