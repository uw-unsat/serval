#lang rosette

(require
  "base.rkt"
  "interp.rkt"
  "decode.rkt"
  (prefix-in core: "../lib/core.rkt")
  racket/list
  racket/match
  racket/port
  racket/string
  syntax/strip-context)

(provide
  (rename-out [literal-read read]
              [literal-read-syntax read-syntax])
  compile-objdump-program
  interpret-objdump-program
  (all-from-out "base.rkt")
  (all-from-out "interp.rkt"))

(define (literal-read in)
  (syntax->datum
    (literal-read-syntax #f in)))

(define (is-branch code)
  (match code
    ; only use "core" branch instrs
    [(or "beq" "bne" "blt" "bge" "bltu" "bgeu") #t]
    [_ #f]))

(define (is-jump code)
  (match code
    ["jal" #t]
    ; jalr is not relative: can be handled like every other instr
    [_ #f]))

(define (parse-operand code idx str)
  (match str

    ; 0xXXXX is always hex
    [(pregexp #px"^0x([0-9a-f]+)$" (list _ num))
      (with-syntax ([n (string->number num 16)])
        #'n)]

    ; The third operand of branches is in hex
    [(pregexp #px"^[0-9a-f]+$" (list num))
         #:when (or (and (= idx 2) (is-branch code))
                    (and (= idx 1) (is-jump code)))
      (with-syntax ([n (string->number num 16)])
        #'n)]

    ; Immediate using only 0-9: probably a base-10 value,
    ; double check that this isn't a branch instr.
    [(pregexp #px"^-?[0-9]+$" (list num))
       (when (or (is-jump code) (is-branch code))
         (error "Base 10 immediate in branch instr"))
       (with-syntax ([n (string->number num 10)])
         #'n)]

    ; Memory offset e.g., "4(a5)"
    [(pregexp #px"^(-?[0-9]+)\\((.+)\\)$" (list _ off reg))
       (with-syntax ([n (string->number off)]
                     [r (string->symbol reg)])
         #'(offset n r))]

    ; Match amo reg address, e.g. "(a5)"
    [(pregexp #px"^\\((.+)\\)$" (list _ reg))
      (with-syntax ([r (string->symbol reg)])
        #'(r))]

    ; Anything else we don't recognize is a register.
    ; Have to be careful here otherwise registers like "a5"
    ; might look like base-16 numbers.
    [else (with-syntax ([s (string->symbol str)]) #'s)]))

(define (read-instructions in)
  (datum->syntax #f
    (for/list ([line (port->lines in)])
      (match line
        [(pregexp #px"([0-9a-f]+) <.+> ([a-f0-9]+)[ \t]+(\\S+)[ \t]*(\\S*)" (list _ addr raw code ops))
           (with-syntax ([a (string->number addr 16)]
                         [os (for/list ([o (string-split ops ",")]
                                        [idx (range 3)])
                               (parse-operand code idx o))]
                         [size (/ (string-length raw) 2)]
                         [o (string->symbol code)]
                         [r raw])
             (strip-context
              #'(a (o . os) #:size size #:raw r)))]))))

(define (read-header in)
  (define line (read-line in))
  (match line
    [(pregexp #px"^architecture: (.+), flags 0x(.+):$" (list _ arch flags))
       (with-syntax ([arch (string->symbol arch)]
                     [fl (string->number flags 16)])
         (append (list #'(define flags 'fl)
                       #'(define architecture 'arch))
                 (read-header in)))]
    [(pregexp #px"Disassembly") null]
    [_ (read-header in)]))

(define (literal-read-syntax src in)
  (define header-stx (datum->syntax #f (read-header in)))

  (strip-context
    #`(module anything racket/base
        (provide (all-defined-out))
        #,@header-stx
        (define instructions '#,(read-instructions in)))))

(define (parse-objdump-instr i #:addr i-addr #:size size #:raw [raw #f])
  (define rawbv (bv (string->number raw 16) (* 8 size)))
  (decode rawbv))

(define (compile-objdump-program instructions)
  (core:bug-on (! (core:concrete?)))
  (core:bug-on (null? instructions) #:msg "Cannot have empty objdump program.")
  (define base (bv (car (list-ref instructions 0)) (XLEN)))
  (define insn-hash
    (for/hash ([insn instructions])
      (define addr (bv (car insn) (XLEN)))
      (define instr
        (match (cdr insn)
          [(list (list i ...) '#:size size '#:raw raw)
            (parse-objdump-instr i #:addr addr #:size size #:raw raw)]
          [(list i ...) #:when (riscv-default-size)
            (parse-objdump-instr i #:addr addr #:size (riscv-default-size))]
          [_
            (core:bug #:msg (format "Bad objdump ~e" instr))]))
      (values addr instr)))
  (program base insn-hash))


(define (interpret-objdump-program cpu instructions)
  (define program (compile-objdump-program instructions))
  (interpret-program cpu program))