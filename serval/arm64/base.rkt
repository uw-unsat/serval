#lang rosette

(require
  "../lib/bvarith.rkt"
  (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out))

; N: negative condition flag
;    1 if the result is negative
; Z: zero condition flag
;    1 if the result is zero
; C: carry condition flag
;    1 if an unsigned overflow occurs
; V: overflow condition flag
;    1 if a signed overflow occurs
(struct nzcv (n z c v) #:transparent)

(define (nzcv->bitvector x)
  (concat (nzcv-n x) (nzcv-z x) (nzcv-c x) (nzcv-v x) (bv 0 28)))

(define (bitvector->nzcv x)
  (nzcv (bit 31 x) (bit 30 x) (bit 29 x) (bit 28 x)))

(define (integer->gpr n)
  (box (bv n 5)))

; ignore v0-v31, fpcr, fpsr, daif
(struct cpu (pc sp xn nzcv memmgr) #:mutable #:transparent
  #:methods core:gen:gen-cpu
  [(define (gen-cpu-memmgr cpu) (cpu-memmgr cpu))
   (define (gen-cpu-pc cpu) (cpu-pc cpu))])

(define cpu-pc-ref cpu-pc)
(define cpu-pc-set! set-cpu-pc!)

(define (cpu-pc-next! cpu [size (bv 4 64)])
  (cpu-pc-set! cpu (bvadd size (cpu-pc-ref cpu))))

; sp is not banked
(define cpu-sp-ref cpu-sp)
(define (cpu-sp-set! cpu v)
  ; zero-extend the result for a 32-bit instruction form
  (set! v (zero-extend v (bitvector 64)))
  (set-cpu-sp! cpu v))

(define (cpu-gpr-ref cpu k)
  (define xn (cpu-xn cpu))
  (define lst
    (cons (cons (integer->gpr 31) (bv 0 64))
          (map (lambda (x) (cons (integer->gpr x) (vector-ref xn x))) (range 31))))
  (cdr (assoc k lst)))

(define (cpu-gpr-set! cpu k v)
  ; zero-extend the result for a 32-bit instruction form
  (set! v (zero-extend v (bitvector 64)))
  (define xn (cpu-xn cpu))
  (define lst
    (cons (cons (integer->gpr 31) void)
          (map (lambda (x) (cons (integer->gpr x) (lambda () (vector-set! xn x v)))) (range 31))))
  (define proc (cdr (assoc k lst)))
  (proc))

(define cpu-nzcv-ref cpu-nzcv)
(define cpu-nzcv-set! set-cpu-nzcv!)

(define (cpu-pstate.n cpu) (nzcv-n (cpu-nzcv-ref cpu)))
(define (cpu-pstate.z cpu) (nzcv-z (cpu-nzcv-ref cpu)))
(define (cpu-pstate.c cpu) (nzcv-c (cpu-nzcv-ref cpu)))
(define (cpu-pstate.v cpu) (nzcv-v (cpu-nzcv-ref cpu)))

(define (init-cpu memmgr)
  (define-symbolic* pc sp (bitvector 64))
  (define-symbolic* xn (bitvector 64) #:length 31)
  (define-symbolic* n z c v (bitvector 1))
  (cpu pc sp (list->vector xn) (nzcv n z c v) memmgr))

(define-generics instruction
  (instruction-encode instruction)
  (instruction-run instruction cpu))

; instruction-run returns void means that the PC should bump to
; the next instruction.
(define (interpret-insn cpu insn)
  ; Bump the pc for most instructions, which return #<void>.
  ; Skip for branching instructions, which return #f.
  (when (instruction-run insn cpu)
    (cpu-pc-next! cpu)))
