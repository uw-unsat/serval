#lang rosette

(require
    (prefix-in core: "lib/core.rkt")
    rosette/base/core/polymorphic)
  
(provide (all-defined-out))

(struct interpreter (func locals stack pc) #:mutable #:transparent)

(define (make-interpreter func args)
  (define nlocals (hash-ref func 'co_nlocals))
  (define locals (vector-append (list->vector args)
                                (make-vector (- nlocals (length args)) #f)))
  (interpreter func locals null 0))

; since Python 3.6, every instruction takes 2 bytes
(define (interpreter-next! interp)
  (set-interpreter-pc! interp (+ 2 (interpreter-pc interp))))

(define (interpreter-jump! interp pc)
  (set-interpreter-pc! interp pc))

(define (interpreter-push! interp val)
  (set-interpreter-stack! interp (cons val (interpreter-stack interp))))

(define (interpreter-pop! interp n)
  (define-values (args stack) (split-at (interpreter-stack interp) n))
  (set-interpreter-stack! interp stack)
  (reverse args))

(define (interpreter-pop1! interp)
  (car (interpreter-pop! interp 1)))

(define (interpret-call args)
  (match args
    [(list 'BitVecVal val n) (bv val n)]
    [(list 'UDiv x y) (bvudiv x y)]
    [_ (core:bug-on #t #:dbg pc #:msg (format "unknown call ~e\n" args))]))

(define (interpret-compare op args)
  (when (not (equal? (length args) 2))
    (core:bug-on #t #:dbg pc #:msg (format "unknown compare args ~e\n" args)))
  (define lhs (first args))
  (define rhs (second args))
  (case op
    [(2) (equal? lhs rhs)]
    [else (core:bug-on #t #:dbg pc #:msg (format "unknown compare op ~e\n" op))]))

(define (interpret-instr interp instr)
  (define pc (interpreter-pc interp))
  (define locals (interpreter-locals interp))
  (define func (interpreter-func interp))
  (define consts (hash-ref func 'co_consts))
  (define names (hash-ref func 'co_names))
  (match instr
    [(list 'CALL_FUNCTION arg)
      (define result (interpret-call (interpreter-pop! interp (add1 arg))))
      (interpreter-push! interp result)
      (interpreter-next! interp)]

    [(list 'COMPARE_OP arg)
      (define result (interpret-compare arg (interpreter-pop! interp 2)))
      (interpreter-push! interp result)
      (interpreter-next! interp)]

    [(list 'LOAD_CONST arg)
      (interpreter-push! interp (vector-ref consts arg))
      (interpreter-next! interp)]

    [(list 'LOAD_FAST arg)
      (interpreter-push! interp (vector-ref locals arg))
      (interpreter-next! interp)]

    [(list 'LOAD_GLOBAL arg)
      (interpreter-push! interp (vector-ref names arg))
      (interpreter-next! interp)]

    [(list 'POP_JUMP_IF_FALSE arg)
      (if (interpreter-pop1! interp)
          (interpreter-next! interp)
          (interpreter-jump! interp arg))]

    [_ (core:bug-on #t #:dbg pc #:msg (format "unknown instruction ~e\n" instr))]))

(define (interpret-program interp)
  (define func (interpreter-func interp))
  (define crunched-pc (interpreter-pc interp))
  (for/all ([pc crunched-pc #:exhaustive]) (begin
    (core:bug-on (not (hash-has-key? func pc))
    #:msg "interpret-program: no instruction found" #:dbg pc)
    (match (hash-ref func pc)
      ; no function call support - terminate for now
      [(list 'RETURN_VALUE)
        (interpreter-pop1! interp)]

      [(list instr ...)
        (interpret-instr interp instr)
        (interpret-program interp)]

      [any (core:bug-on #t #:dbg pc #:msg (format "Bad instruction format ~e\n" any))]))))
