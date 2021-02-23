#lang rosette

(require
  (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out))

(struct pstate (n z c v) #:mutable #:transparent)

(define (pstate.nzcv-set! pstate nzcv)
  (set-pstate-n! pstate (first nzcv))
  (set-pstate-z! pstate (second nzcv))
  (set-pstate-c! pstate (third nzcv))
  (set-pstate-v! pstate (fourth nzcv)))

(define (pstate->bitvector x)
  ; set [4:0] to supervisor for simplicity
  (concat (pstate-n x) (pstate-z x) (pstate-c x) (pstate-v x) (bv 0 23) (bv #b10011 5)))

(define (bitvector->pstate x)
  (pstate (bit 31 x) (bit 30 x) (bit 29 x) (bit 28 x)))

(define (integer->gpr n)
  (box (bv n 4)))

(struct cpu (pc rs cpsr memmgr) #:mutable #:transparent
  #:methods core:gen:gen-cpu
  [(define (gen-cpu-memmgr cpu) (cpu-memmgr cpu))
   (define (gen-cpu-pc cpu) (cpu-pc cpu))])

(define (cpu-pc-set! cpu v)
  (set-cpu-pc! cpu v))

(define (cpu-pc-next! cpu [size (bv 4 32)])
  (for/all ([pc (cpu-pc cpu) #:exhaustive])
    (cpu-pc-set! cpu (bvadd size pc))))

; r15 is read as PC + 8.
(define (cpu-gpr-ref cpu k)
  (define rs (cpu-rs cpu))
  (define lst
    (cons (cons (integer->gpr 15) (bvadd (cpu-pc cpu) (bv 8 32)))
          (map (lambda (x) (cons (integer->gpr x) (vector-ref rs x))) (range 15))))
  (cdr (assoc k lst)))

; PC must be modified explicitly using cpu-pc-set! rather than r15.
(define (cpu-gpr-set! cpu k v)
  (define rs (cpu-rs cpu))
  (define lst
    (map (lambda (x) (cons (integer->gpr x) (lambda () (vector-set! rs x v)))) (range 15)))
  (define proc (cdr (assoc k lst)))
  (proc))

(define (cpu-pstate.nzcv-set! cpu nzcv)
  (pstate.nzcv-set! (cpu-cpsr cpu) nzcv))

(define (cpu-pstate.n cpu) (pstate-n (cpu-cpsr cpu)))
(define (cpu-pstate.z cpu) (pstate-z (cpu-cpsr cpu)))
(define (cpu-pstate.c cpu) (pstate-c (cpu-cpsr cpu)))
(define (cpu-pstate.v cpu) (pstate-v (cpu-cpsr cpu)))

(define (cpu-pstate.n-set! cpu b) (set-pstate-n! (cpu-cpsr cpu) b))
(define (cpu-pstate.z-set! cpu b) (set-pstate-z! (cpu-cpsr cpu) b))
(define (cpu-pstate.c-set! cpu b) (set-pstate-c! (cpu-cpsr cpu) b))
(define (cpu-pstate.v-set! cpu b) (set-pstate-v! (cpu-cpsr cpu) b))

(define (init-cpu memmgr)
  (define-symbolic* pc (bitvector 32))
  (define-symbolic* rs (bitvector 32) #:length 15)
  (define-symbolic* n z c v (bitvector 1))
  (define cpsr (pstate n z c v))
  (cpu pc (list->vector rs) cpsr memmgr))

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


; conditional instructions

(define (condition-holds cpu cc)
  ; Evaluate base condition.
  (define cond<3:1> (extract 3 1 cc))
  (define result
    (cond
      [(bveq cond<3:1> (bv #b000 3)) (bveq (cpu-pstate.z cpu) (bv #b1 1))]
      [(bveq cond<3:1> (bv #b001 3)) (bveq (cpu-pstate.c cpu) (bv #b1 1))]
      [(bveq cond<3:1> (bv #b010 3)) (bveq (cpu-pstate.n cpu) (bv #b1 1))]
      [(bveq cond<3:1> (bv #b011 3)) (bveq (cpu-pstate.v cpu) (bv #b1 1))]
      [(bveq cond<3:1> (bv #b100 3)) (&& (bveq (cpu-pstate.c cpu) (bv #b1 1)) (bveq (cpu-pstate.z cpu) (bv #b0 1)))]
      [(bveq cond<3:1> (bv #b101 3)) (bveq (cpu-pstate.n cpu) (cpu-pstate.v cpu))]
      [(bveq cond<3:1> (bv #b110 3)) (&& (bveq (cpu-pstate.n cpu) (cpu-pstate.v cpu)) (bveq (cpu-pstate.z cpu) (bv #b0 1)))]
      [(bveq cond<3:1> (bv #b111 3)) #t]))

  ; Condition flag values in the set '111x' indicate always true.
  ; Otherwise, invert condition if necessary.
  (if (&& (bveq (bit 0 cc) (bv #b1 1)) (! (bveq cc (bv #b1111 4))))
      (! result)
      result))

(define (conditional-instruction-encode insn)
  (concat (conditional-instruction-cond insn)
          (instruction-encode (conditional-instruction-raw insn))))

(define (conditional-instruction-run insn cpu)
  (when (condition-holds cpu (conditional-instruction-cond insn))
    (instruction-run (conditional-instruction-raw insn) cpu)))

(struct conditional-instruction (cond raw)
  #:transparent
  #:methods gen:instruction
  [(define instruction-encode conditional-instruction-encode)
   (define instruction-run conditional-instruction-run)])
