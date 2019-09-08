#lang rosette

(require
  "base.rkt"
  "interp.rkt"
  "encoder.rkt"
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
  (define insn
   (match i

    ; No arguments
    [(list (and op (or 'c.unimp 'unimp 'mret 'nop 'wfi 'sfence.vma 'fence.i 'fence 'ecall 'ebreak)) _ ...)
      (instr op #f #f #f #f size)]

    ; Dst + 20-bit imm
    [(list (and op (or 'auipc 'lui)) dst off)
      (instr op dst #f #f (bv off 20) size)]

    ; dst + src + 12-bit imm
    [(list (and op (or 'sltiu 'addiw 'slliw 'srliw 'sraiw 'addi 'subi 'muli 'ori 'andi 'xori 'srli 'srai 'slli)) dst src imm)
      (instr op dst src #f (bv imm 12) size)]

    ; dst + src + src
    [(list (and op (or 'sltu 'addw 'add 'subw 'sub 'or 'and 'xor 'srlw 'srl 'sraw 'sra 'sllw 'sll 'mulw 'mul 'mulh 'mulhu 'mulhsu 'divw 'div 'remw 'rem 'divuw 'divu 'remuw 'remu)) dst src1 src2)
      (instr op dst src1 src2 #f size)]

    [(list 'jal dst abs-addr)

      (define off (extract 20 1 (bvsub (bv abs-addr (XLEN)) i-addr)))
      (core:bug-on
        (! (bveq (bv abs-addr (XLEN))
                 (bvadd i-addr (bvshl (sign-extend off (bitvector (XLEN))) (bv 1 (XLEN))))))
        #:msg (format "Relinking failed:\n i-addr ~e\n off ~e" i-addr off))

      (instr 'jal dst #f #f off size)]

    [(list 'jalr dst (list 'offset imm src))
      (instr 'jalr dst src #f (bv imm 12) size)]

    [(list (and op (or 'ld 'ldu 'lw 'lwu 'lh 'lhu 'lb 'lbu))
           dst
           (list 'offset off reg))
      (instr op dst reg #f (bv off 12) size)]

    [(list (and op (or 'sd 'sw 'sh 'sb))
           src
           (list 'offset off reg))
      (instr op #f reg src (bv off 12) size)]

    [(list (and op (or 'amoor.w.aq))
           dst
           src
           (list addr))
      (instr op dst src addr #f size)]

    [(list (and op (or 'bge 'blt 'bgeu 'bltu 'bne 'beq)) reg1 reg2 abs-addr)
      (define off (extract 12 1 (bvsub (bv abs-addr (XLEN)) i-addr)))
      (core:bug-on
        (! (bveq (bv abs-addr (XLEN))
                 (bvadd i-addr (bvshl (sign-extend off (bitvector (XLEN))) (bv 1 (XLEN))))))
        #:msg (format "Relinking failed:\n i-addr ~e\n off ~e" i-addr off))

    (instr op #f reg1 reg2 off size)]

    [(list (and op (or 'csrrw 'csrrs 'csrrc)) dst csr src)
      (instr op dst csr src #f size)]

    [(list (and op (or 'csrrwi 'csrrsi 'csrrci)) dst csr imm)
      (instr op dst csr #f (bv imm 5) size)]

    [else (core:bug-on #t #:msg (format "Bad parse ~e" i))]))

  (when raw
    (define encoded (encode-instr insn))
    (when encoded
      (define rawbv (bv (string->number raw 16) (* 8 size)))
      (assert (equal? rawbv encoded) (format "encoding failed: ~a ~a ~a" insn rawbv encoded))))

  insn)

(define (compile-objdump-program instructions)
  (core:bug-on (! (core:concrete?)))
  (when (null? instructions)
    (error "Cannot have empty objdump program"))
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
            (core:bug-on #t #:dbg current-pc-debug #:msg (format "Bad objdump ~e" instr))]))
      (values addr instr)))
  (program base insn-hash))


(define (interpret-objdump-program cpu instructions)
  (define program (compile-objdump-program instructions))
  (interpret-program cpu program))