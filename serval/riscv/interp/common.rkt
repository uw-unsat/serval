#lang rosette

(require
  "define.rkt"
  "../base.rkt"
  "../../lib/bvarith.rkt"
  (prefix-in core: "../../lib/core.rkt"))

(provide
  (all-defined-out)
  (all-from-out
    "define.rkt"
    "../base.rkt"
    "../../lib/bvarith.rkt"
    "../../lib/core.rkt"))

(struct ptr (addr off size) #:transparent)

(define (notimplemented cpu insn . args)
  (error (format "instruction ~v not implemented" insn)))

(define (instruction-size insn)
  (for/all ([insn insn #:exhaustive])
    (/ (core:bv-size (instruction-encode insn)) 8)))

(define (cpu-next! cpu insn)
  (set-cpu-pc! cpu (bvadd (bv (instruction-size insn) (cpu-xlen cpu))
                          (cpu-pc cpu))))

(define (skip cpu insn . args)
  (cpu-next! cpu insn))

(define (skip/debug cpu insn . args)
  (displayln insn)
  (cpu-next! cpu insn))

; Make a shift op from SMT shift operation by masking out upper bits.
(define ((make-shift-op op) v1 v2)
  (op v1 (bvand (bv (sub1 (core:bv-size v1)) (core:bv-size v1)) v2)))

; Make a comparison op from SMT comparison and lifting to bv.
(define ((make-cmp-op op) v1 v2)
  (if (op v1 v2) (bv 1 (core:bv-size v1)) (bv 0 (core:bv-size v1))))

; Register-register operation
(define (reg-reg-op op cpu insn rs2 rs1 rd)
  (define a (gpr-ref cpu rs1))
  (define b (gpr-ref cpu rs2))
  (gpr-set! cpu rd (op a b))
  (cpu-next! cpu insn))

(define (reg-imm-op op cpu insn imm rs1 rd)
  (define xlen (cpu-xlen cpu))
  (define a (gpr-ref cpu rs1))
  (define b (sign-extend imm (bitvector xlen)))
  (gpr-set! cpu rd (op a b))
  (cpu-next! cpu insn))

; 32-bit ops on rv64
(define (reg-reg-opw op cpu insn rs2 rs1 rd)
  (core:bug-on (! (= (cpu-xlen cpu) 64)) #:msg (format "~v: (cpu-xlen cpu) != 64" insn)
                                         #:dbg current-pc-debug)
  (define a (trunc 32 (gpr-ref cpu rs1)))
  (define b (trunc 32 (gpr-ref cpu rs2)))
  (gpr-set! cpu rd (sign-extend (op a b) (bitvector 64)))
  (cpu-next! cpu insn))

(define (reg-imm-opw op cpu insn imm rs1 rd)
  (core:bug-on (! (= (cpu-xlen cpu) 64)) #:msg (format "~v: (cpu-xlen cpu) != 64" insn)
                                         #:dbg current-pc-debug)
  (define a (trunc 32 (gpr-ref cpu rs1)))
  (define b (sign-extend imm (bitvector 32)))
  (gpr-set! cpu rd (sign-extend (op a b) (bitvector 64)))
  (cpu-next! cpu insn))

(define (encode-compressed-gpr gpr)
  (case gpr
    [(s0 fp x8) (bv #b000 3)]
    [(s1 x9)    (bv #b001 3)]
    [(a0 x10)   (bv #b010 3)]
    [(a1 x11)   (bv #b011 3)]
    [(a2 x12)   (bv #b100 3)]
    [(a3 x13)   (bv #b101 3)]
    [(a4 x14)   (bv #b110 3)]
    [(a5 x15)   (bv #b111 3)]
    [else (core:bug #:dbg current-pc-debug
                    #:msg (format "cannot encode ~v as compressed gpr" gpr))]))

(define (decode-compressed-gpr gpr)
  (for/all ([gpr gpr #:exhaustive])
    (cond
      [(bveq gpr (bv #b000 3)) 's0]
      [(bveq gpr (bv #b001 3)) 's1]
      [(bveq gpr (bv #b010 3)) 'a0]
      [(bveq gpr (bv #b011 3)) 'a1]
      [(bveq gpr (bv #b100 3)) 'a2]
      [(bveq gpr (bv #b101 3)) 'a3]
      [(bveq gpr (bv #b110 3)) 'a4]
      [(bveq gpr (bv #b111 3)) 'a5]
      [else
        (core:bug #:dbg current-pc-debug
                  #:msg (format "No such compressed GPR ~e\n" gpr))])))
