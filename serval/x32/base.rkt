#lang rosette

(require (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out))

(struct gprs (eax ecx edx ebx esp ebp esi edi) #:mutable #:transparent)
(struct flags (cf pf af zf sf of) #:mutable #:transparent)

(struct cpu (pc gprs flags mregions) #:mutable #:transparent)

(define (init-cpu [symbols null] [globals null])
  (define-symbolic* eax ecx edx ebx esp ebp esi edi (bitvector 32))
  (define-symbolic* cf pf af zf sf of boolean?)
  (define mregions (core:create-mregions symbols globals))
  (define reset-vector (bv 0 32))
  (cpu reset-vector (gprs eax ecx edx ebx esp ebp esi edi) (flags cf pf af zf sf of) mregions))

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
     (core:bug
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
  (define (gpr-update-16 old new) (concat (extract 31 16 old) new))
  (define (gpr-update-8h old new) (concat (extract 31 16 old) new (extract 7 0 old)))
  (define (gpr-update-8l old new) (concat (extract 31 8 old) new))
  (define rs (cpu-gprs cpu))
  (case gpr
    [(eax) (set-gprs-eax! rs val)]
    [(ecx) (set-gprs-ecx! rs val)]
    [(edx) (set-gprs-edx! rs val)]
    [(ebx) (set-gprs-ebx! rs val)]
    [(esp) (set-gprs-esp! rs val)]
    [(ebp) (set-gprs-ebp! rs val)]
    [(esi) (set-gprs-esi! rs val)]
    [(edi) (set-gprs-edi! rs val)]
    ;
    [(ax) (set-gprs-eax! rs (gpr-update-16 (gprs-eax rs) val))]
    [(cx) (set-gprs-ecx! rs (gpr-update-16 (gprs-ecx rs) val))]
    [(dx) (set-gprs-edx! rs (gpr-update-16 (gprs-edx rs) val))]
    [(bx) (set-gprs-ebx! rs (gpr-update-16 (gprs-ebx rs) val))]
    [(sp) (set-gprs-esp! rs (gpr-update-16 (gprs-esp rs) val))]
    [(bp) (set-gprs-ebp! rs (gpr-update-16 (gprs-ebp rs) val))]
    [(si) (set-gprs-esi! rs (gpr-update-16 (gprs-esi rs) val))]
    [(di) (set-gprs-edi! rs (gpr-update-16 (gprs-edi rs) val))]
    ;
    [(ah) (set-gprs-eax! rs (gpr-update-8h (gprs-eax rs) val))]
    [(ch) (set-gprs-ecx! rs (gpr-update-8h (gprs-ecx rs) val))]
    [(dh) (set-gprs-edx! rs (gpr-update-8h (gprs-edx rs) val))]
    [(bh) (set-gprs-ebx! rs (gpr-update-8h (gprs-ebx rs) val))]
    ;
    [(al) (set-gprs-eax! rs (gpr-update-8l (gprs-eax rs) val))]
    [(cl) (set-gprs-ecx! rs (gpr-update-8l (gprs-ecx rs) val))]
    [(dl) (set-gprs-edx! rs (gpr-update-8l (gprs-edx rs) val))]
    [(bl) (set-gprs-ebx! rs (gpr-update-8l (gprs-ebx rs) val))]
    [(spl) (set-gprs-esp! rs (gpr-update-8l (gprs-esp rs) val))]
    [(bpl) (set-gprs-ebp! rs (gpr-update-8l (gprs-ebp rs) val))]
    [(sil) (set-gprs-esi! rs (gpr-update-8l (gprs-esi rs) val))]
    [(dil) (set-gprs-edi! rs (gpr-update-8l (gprs-edi rs) val))]
    ;
    [else (core:bug #:msg (format "gpr-set!: unknown gpr ~e" gpr) #:dbg current-pc-debug)]))

(define (gpr-ref cpu gpr)
  (define (gpr-ref-16 x) (extract 15 0 x))
  (define (gpr-ref-8h x) (extract 15 8 x))
  (define (gpr-ref-8l x) (extract 7 0 x))
  (define rs (cpu-gprs cpu))
  (case gpr
    [(eax) (gprs-eax rs)]
    [(ecx) (gprs-ecx rs)]
    [(edx) (gprs-edx rs)]
    [(ebx) (gprs-ebx rs)]
    [(esp) (gprs-esp rs)]
    [(ebp) (gprs-ebp rs)]
    [(esi) (gprs-esi rs)]
    [(edi) (gprs-edi rs)]
    ;
    [(ax) (gpr-ref-16 (gprs-eax rs))]
    [(cx) (gpr-ref-16 (gprs-ecx rs))]
    [(dx) (gpr-ref-16 (gprs-edx rs))]
    [(bx) (gpr-ref-16 (gprs-ebx rs))]
    [(sp) (gpr-ref-16 (gprs-esp rs))]
    [(bp) (gpr-ref-16 (gprs-ebp rs))]
    [(si) (gpr-ref-16 (gprs-esi rs))]
    [(di) (gpr-ref-16 (gprs-edi rs))]
    ;
    [(ah) (gpr-ref-8h (gprs-eax rs))]
    [(ch) (gpr-ref-8h (gprs-ecx rs))]
    [(dh) (gpr-ref-8h (gprs-edx rs))]
    [(bh) (gpr-ref-8h (gprs-ebx rs))]
    ;
    [(al) (gpr-ref-8l (gprs-eax rs))]
    [(cl) (gpr-ref-8l (gprs-ecx rs))]
    [(dl) (gpr-ref-8l (gprs-edx rs))]
    [(bl) (gpr-ref-8l (gprs-ebx rs))]
    [(spl) (gpr-ref-8l (gprs-esp rs))]
    [(bpl) (gpr-ref-8l (gprs-ebp rs))]
    [(sil) (gpr-ref-8l (gprs-esi rs))]
    [(dil) (gpr-ref-8l (gprs-edi rs))]
    ;
    [else (core:bug #:msg (format "gpr-ref: unknown gpr ~e" gpr) #:dbg current-pc-debug)]))

(define (flag->idx flag)
  (case flag
    [(CF) 0]  ; carry flag
    [(PF) 2]  ; parity flag
    [(AF) 4]  ; auxiliary carry flag
    [(ZF) 6]  ; zero flag
    [(SF) 7]  ; sign flag
    [(OF) 11] ; overflow flag
    [else
     (core:bug
      #:msg (format "flag->idx: unknown flag: ~e" flag)
      #:dbg current-pc-debug)]))

(define (flag-set! cpu flag [val #t])
  (core:bug-on (! (boolean? val))
   #:msg (format "flag-set!: not boolean: ~e" val)
   #:dbg current-pc-debug)
  (define fs (cpu-flags cpu))
  (case flag
    [(CF) (set-flags-cf! fs val)]
    [(PF) (set-flags-pf! fs val)]
    [(AF) (set-flags-af! fs val)]
    [(ZF) (set-flags-zf! fs val)]
    [(SF) (set-flags-sf! fs val)]
    [(OF) (set-flags-of! fs val)]
    [else
     (core:bug
      #:msg (format "flag->set!: unknown flag: ~e" flag)
      #:dbg current-pc-debug)]))

(define (flag-clear! cpu flag)
  (flag-set! cpu flag #f))

(define (parity x)
  (bvnot (apply bvxor (for/list ([i (in-range (core:bv-size x))]) (extract i i x)))))

(define (flag-set-result! cpu val)
  (core:bug-on (! ((bitvector 32) val))
   #:msg (format "flag-set!: not bv32: ~e" val)
   #:dbg current-pc-debug)
  (flag-set! cpu 'PF (core:bitvector->bool (parity (extract 7 0 val))))
  (flag-set! cpu 'ZF (core:bvzero? val))
  (flag-set! cpu 'SF (core:bitvector->bool (core:msb val))))

(define (flag-havoc! cpu flag)
  (define-symbolic* havoc boolean?)
  (flag-set! cpu flag havoc))

(define (@flag-ref fs flag)
  (case flag
    [(CF) (flags-cf fs)]
    [(PF) (flags-pf fs)]
    [(AF) (flags-af fs)]
    [(ZF) (flags-zf fs)]
    [(SF) (flags-sf fs)]
    [(OF) (flags-of fs)]
    [else
     (core:bug
      #:msg (format "flag->ref: unknown flag: ~e" flag)
      #:dbg current-pc-debug)]))

(define (flag-ref cpu flag)
  (@flag-ref (cpu-flags cpu) flag))

(define (flags->bitvector fs)
  (define (flag->bv32 flag)
    (if (@flag-ref fs flag) (bv (arithmetic-shift 1 (flag->idx flag)) 32) (bv 0 32)))
  (apply bvor (map flag->bv32 '(CF PF AF ZF SF OF))))

(define (bitvector->flags v)
  (define lst
    (for/list ([flag '(CF PF AF ZF SF OF)])
      (define i (flag->idx flag))
      (core:bitvector->bool (extract i i v))))
  (apply flags lst))

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
       (hex-symbol->integer x)]
      [else (core:bug #:msg "instruction->integer-bytes: ->list failed with x = ~v" x)]))
  (flatten (map ->list code)))

(struct program (base instructions) #:transparent)
