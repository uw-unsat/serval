#lang rosette

(require
  "common.rkt")

(provide
  and-register ands-register
  eor-register eors-register
  sub-register subs-register
  rsb-register rsbs-register
  add-register adds-register
  adc-register adcs-register
  sbc-register sbcs-register
  rsc-register rscs-register)


(define (decode S Rn Rd imm5 stype Rm)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define setflags (bveq S (bv #b1 1)))
  (define-values (shift_t shift_n) (decode-imm-shift stype imm5))
  (values d n m setflags shift_t shift_n))

(define (interpret-bitwise proc cpu S Rn Rd imm5 stype Rm)
  (define-values (d n m setflags shift_t shift_n) (decode S Rn Rd imm5 stype Rm))
  (define-values (shifted carry) (shift_c (cpu-gpr-ref cpu m) shift_t shift_n (cpu-pstate.c cpu)))
  (define result (proc (cpu-gpr-ref cpu n) shifted))
  (cond
    [(r15? d)
     (if setflags
         (alu-exception-return cpu result)
         (alu-write-pc cpu result))]
    [else
     (cpu-gpr-set! cpu d result)
     (when setflags
       (cpu-pstate.n-set! cpu (bit 31 result))
       (cpu-pstate.z-set! cpu (is-zero-bit result))
       (cpu-pstate.c-set! cpu carry)
       ; PSTATE.V
     )]))

(define interpret-and/ands
  (curry interpret-bitwise bvand))

(define interpret-eor/eors
  (curry interpret-bitwise bvxor))

(define (interpret-arithmetic proc-Rn proc-shifted proc-carry cpu S Rn Rd imm5 stype Rm)
  (define-values (d n m setflags shift_t shift_n) (decode S Rn Rd imm5 stype Rm))
  (define shifted (shift (cpu-gpr-ref cpu m) shift_t shift_n (cpu-pstate.c cpu)))
  (define-values (result nzcv) (add-with-carry (proc-Rn (cpu-gpr-ref cpu n)) (proc-shifted shifted) (proc-carry (cpu-pstate.c cpu))))
  (cond
    [(r15? d)
     (if setflags
         (alu-exception-return cpu result)
         (alu-write-pc cpu result))]
    [else
     (cpu-gpr-set! cpu d result)
     (when setflags
       (cpu-pstate.nzcv-set! cpu nzcv))]))

(define interpret-sub/subs
  (curry interpret-arithmetic identity bvnot (const (bv #b1 1))))

(define interpret-rsb/rsbs
  (curry interpret-arithmetic bvnot identity (const (bv #b1 1))))

(define interpret-add/adds
  (curry interpret-arithmetic identity identity (const (bv #b0 1))))

(define interpret-adc/adcs
  (curry interpret-arithmetic identity identity identity))

(define interpret-sbc/sbcs
  (curry interpret-arithmetic identity bvnot identity))

(define interpret-rsc/rscs
  (curry interpret-arithmetic bvnot identity identity))


(define-insn (S Rn Rd imm5 stype Rm)
  #:encode (lambda (opc) (list (bv #b0000 4) (bv opc 3) S Rn Rd imm5 stype (bv #b0 1) Rm))
  [(#b000) and/ands-register interpret-and/ands]
  [(#b001) eor/eors-register interpret-eor/eors]
  [(#b010) sub/subs-register interpret-sub/subs]
  [(#b011) rsb/rsbs-register interpret-rsb/rsbs]
  [(#b100) add/adds-register interpret-add/adds]
  [(#b101) adc/adcs-register interpret-adc/adcs]
  [(#b110) sbc/sbcs-register interpret-sbc/sbcs]
  [(#b111) rsc/rscs-register interpret-rsc/rscs])

(define and-register
  (curry and/ands-register (bv #b0 1)))

(define ands-register
  (curry and/ands-register (bv #b1 1)))

(define eor-register
  (curry eor/eors-register (bv #b0 1)))

(define eors-register
  (curry eor/eors-register (bv #b1 1)))

(define sub-register
  (curry sub/subs-register (bv #b0 1)))

(define subs-register
  (curry sub/subs-register (bv #b1 1)))

(define rsb-register
  (curry rsb/rsbs-register (bv #b0 1)))

(define rsbs-register
  (curry rsb/rsbs-register (bv #b1 1)))

(define add-register
  (curry add/adds-register (bv #b0 1)))

(define adds-register
  (curry add/adds-register (bv #b1 1)))

(define adc-register
  (curry adc/adcs-register (bv #b0 1)))

(define adcs-register
  (curry adc/adcs-register (bv #b1 1)))

(define sbc-register
  (curry sbc/sbcs-register (bv #b0 1)))

(define sbcs-register
  (curry sbc/sbcs-register (bv #b1 1)))

(define rsc-register
  (curry rsc/rscs-register (bv #b0 1)))

(define rscs-register
  (curry rsc/rscs-register (bv #b1 1)))
