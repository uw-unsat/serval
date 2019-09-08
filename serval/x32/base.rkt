#lang rosette

(require (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out))


(struct cpu (pc gprs flags mregions) #:mutable #:transparent)

(define (init-cpu [symbols null] [globals null])
  (define-symbolic* gprs (bitvector 32) [8])
  (define-symbolic* cf pf af zf sf of boolean?)
  (define flags (vector cf #t pf #f af #f zf sf #f #f #f of #f #f #f #f
                        #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
  (define mregions (core:create-mregions symbols globals))
  (define reset-vector (bv 0 32))
  (cpu reset-vector (apply vector gprs) flags mregions))

(define (cpu-next! cpu size)
  (set-cpu-pc! cpu (bvadd size (cpu-pc cpu))))


(define current-pc-debug #f)

(define (set-current-pc-debug! v)
  (set! current-pc-debug v))


(define (gpr->idx gpr)
  (case gpr
    [(eax ax al) 0]
    [(ecx cx cl) 1]
    [(edx dl) 2]
    [(ebx bl) 3]
    [(esp ah) 4]
    [(ebp ch) 5]
    [(esi dh) 6]
    [(edi bh) 7]
    [else
     (core:bug-on #t
      #:msg (format "gpr->idx: unknown gpr: ~e" gpr)
      #:dbg current-pc-debug)]))

(define (gpr-full gpr)
  (case gpr
    [(ax ah al) 'eax]
    [(cx ch cl) 'ecx]
    [(dx dh dl) 'edx]
    [(bx bh bl) 'ebx]
    [(sp spl) 'esp]
    [(bp bpl) 'ebp]
    [(si sil) 'esi]
    [(di dil) 'edi]
    [else gpr]))

(define (gpr-set! cpu gpr val)
  (define gprs (cpu-gprs cpu))
  (define idx (gpr->idx (gpr-full gpr)))
  (define oldval (vector-ref gprs idx))
  (define newval
    (case gpr
      [(ax cx dx bx sp bp si di)
       (concat (extract 31 16 oldval) val)]
      [(ah ch dh bh)
       (concat (extract 31 16 oldval) val (extract 7 0 oldval))]
      [(al cl dl bl spl bpl sil dil)
       (concat (extract 31 8 oldval) val)]
      [else val]))
  (core:bug-on (not ((bitvector 32) newval))
   #:msg (format "gpr-set! ~a mismatch: ~a" gpr val)
   #:dbg current-pc-debug)
  (vector-set! gprs idx newval))

(define (gpr-ref cpu gpr)
  (define val (vector-ref (cpu-gprs cpu) (gpr->idx (gpr-full gpr))))
  (case gpr
    [(ax cx dx bx sp bp si di) (extract 15 0 val)]
    [(ah ch dh bh) (extract 15 8 val)]
    [(al cl dl bl spl bpl sil dil) (extract 7 0 val)]
    [else val]))

(define (flag->idx flag)
  (case flag
    [(CF) 0]  ; carry flag
    [(PF) 2]  ; parity flag
    [(AF) 4]  ; auxiliary carry flag
    [(ZF) 6]  ; zero flag
    [(SF) 7]  ; sign flag
    [(OF) 11] ; overflow flag
    [else
     (core:bug-on #t
      #:msg (format "flag->idx: unknown flag: ~e" flag)
      #:dbg current-pc-debug)]))

(define (flag-set! cpu flag [val #t])
  (core:bug-on (not (boolean? val))
   #:msg (format "flag-set!: not boolean: ~e" val)
   #:dbg current-pc-debug)
  (vector-set! (cpu-flags cpu) (flag->idx flag) val))

(define (flag-clear! cpu flag)
  (flag-set! cpu flag #f))

(define (parity x)
  (bvnot (apply bvxor (for/list ([i (in-range (core:bv-size x))]) (extract i i x)))))

(define (flag-set-result! cpu val)
  (core:bug-on (not ((bitvector 32) val))
   #:msg (format "flag-set!: not bv32: ~e" val)
   #:dbg current-pc-debug)
  (flag-set! cpu 'PF (core:bitvector->bool (parity (extract 7 0 val))))
  (flag-set! cpu 'ZF (core:bvzero? val))
  (flag-set! cpu 'SF (core:bitvector->bool (core:msb val))))

(define (flag-havoc! cpu flag)
  (define-symbolic* havoc boolean?)
  (flag-set! cpu flag havoc))

(define (flag-ref cpu flag)
  (vector-ref (cpu-flags cpu) (flag->idx flag)))


(struct ModOpcodeR/M (mod+opcode r/m) #:transparent)
(struct ModR/M (mod r/m reg) #:transparent)

(define-generics instruction
  (instruction-encode instruction)
  (instruction-run instruction cpu))

(define (instruction-size insn)
  (define (size-of x)
    (cond
      [(bv? x) (/ (core:bv-size x) 8)]
      [else 1]))
  (apply + (map size-of (instruction-encode insn))))

(define (instruction->integer-bytes insn)
  (define code (instruction-encode insn))
  (define (hex-symbol->integer x)
    (string->number (substring (symbol->string x) 2) 16))
  (define (->list x)
    (cond
      [(bv? x)
       (map bitvector->natural (core:bitvector->list/le x))]
      [(ModOpcodeR/M? x)
       (bitwise-ior
         (hex-symbol->integer (ModOpcodeR/M-mod+opcode x))
         (gpr->idx (ModOpcodeR/M-r/m x)))]
      [(ModR/M? x)
       (bitwise-ior
         (hex-symbol->integer (ModR/M-mod x))
         (gpr->idx (ModR/M-r/m x))
         (arithmetic-shift (gpr->idx (ModR/M-reg x)) 3))]
      [(symbol? x)
       (hex-symbol->integer x)]))
  (flatten (map ->list code)))


(struct program (base instructions) #:transparent)
