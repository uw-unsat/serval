#lang rosette

(require
  "../lib/bvarith.rkt"
  (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out) (all-from-out "../lib/bvarith.rkt"))

(define-generics register
  (register-size register)
  (register-name register)
  (register-encode register)
  (register-ref cpu register)
  (register-set! cpu register v))

(define flags '(CF #f PF #f AF #f ZF SF #f #f #f OF #f #f #f #f))

(define (integer->flag x)
  (list-ref flags x))

(define (flag->integer r)
  (index-of flags r))

(struct cpu (pc gprs flags memmgr) #:mutable #:transparent
  #:methods core:gen:gen-cpu
  [(define (gen-cpu-memmgr cpu) (cpu-memmgr cpu))
   (define (gen-cpu-pc cpu)
     (define pc (cpu-pc cpu))
      (define mm (cpu-memmgr cpu))
      (when mm
        (define n (core:memmgr-bitwidth mm))
        ; split before truncation
        (set! pc (for/all ([pc pc #:exhaustive]) (trunc n pc))))
      pc)])

(define (cpu-pc-ref cpu) (core:gen-cpu-pc cpu))

(define (cpu-pc-set! cpu pc)
  (set-cpu-pc! cpu (zero-extend pc (bitvector 64))))

(define (cpu-pc-next! cpu off)
  ; split the pc before add
  (for/all ([pc (cpu-pc-ref cpu) #:exhaustive])
    ; truncate off before bumping the pc (better for 32-bit)
    (set! off (trunc (core:bv-size pc) off))
    (cpu-pc-set! cpu (bvadd pc off))))

(define (cpu-gpr-ref cpu k)
  (register-ref cpu k))

(define (cpu-gpr-set! cpu k v)
  (assert ((bitvector (register-size k)) v)
          (format "cpu-gpr-set!: must be of size ~a: ~a" (register-size k) v))
  (register-set! cpu k v))

(define (cpu-flag-ref cpu k)
  (define lst
    (for/list ([r flags]
               [i (range (length flags))])
      (cons r (lambda (vec) (vector-ref vec i)))))
  (define proc (cdr (assoc k lst)))
  (proc (cpu-flags cpu)))

(define (cpu-flag-set! cpu k v)
  (define lst
    (for/list ([r flags]
               [i (range (length flags))]
               #:when r)
      (cons r (lambda (vec) (vector-set! vec i v)))))
  (define proc (cdr (assoc k lst)))
  (proc (cpu-flags cpu)))

(define (cpu-flag-clear! cpu . ks)
  (for ([k ks])
    (cpu-flag-set! cpu k (bv #b0 1))))

(define (cpu-flag-havoc! cpu . ks)
  (for ([k ks])
    (cpu-flag-set! cpu k #f)))

(define (init-cpu memmgr)
  (define-symbolic* pc (bitvector 64))
  (define-symbolic* gprs (bitvector 64) #:length 16)
  (define-symbolic* cf pf af zf sf of (bitvector 1))
  (define flags (vector-append
    (vector cf (bv #b1 1) pf (bv #b0 1) af (bv #b0 1) zf sf (bv #b0 1) (bv #b0 1) (bv #b0 1) of (bv #b0 1) (bv #b0 1) (bv #b0 1) (bv #b0 1))
    (make-vector 16 (bv #b0 1))))
  (cpu pc (list->vector gprs) flags memmgr))

(define-generics instruction
  (instruction-encode instruction)
  (instruction-run instruction cpu))

(define (interpret-insn cpu insn)
  (instruction-run insn cpu)
  ; bump the pc
  (cpu-pc-next! cpu (bv (instruction-size insn) 64)))

(define (instruction-size insn)
  (length (instruction-encode insn)))
