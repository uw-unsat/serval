#lang rosette

(require
  "common.rkt")

(provide
  and-immediate ands-immediate
  eor-immediate eors-immediate
  sub-immediate subs-immediate
  rsb-immediate rsbs-immediate
  add-immediate adds-immediate
  adc-immediate adcs-immediate
  sbc-immediate sbcs-immediate
  rsc-immediate rscs-immediate)


(define (decode cpu S Rn Rd imm12)
  (define d Rd)
  (define n Rn)
  (define setflags (bveq S (bv #b1 1)))
  (define-values (imm32 carry) (a32-expand-imm_c imm12 (cpu-pstate.c cpu)))
  (values d n setflags imm32 carry))

(define (interpret-bitwise proc cpu S Rn Rd imm12)
  (define-values (d n setflags imm32 carry) (decode cpu S Rn Rd imm12))
  (define result (proc (cpu-gpr-ref cpu n) imm32))
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
       ; PSTATE.V unchanged
     )]))

(define interpret-and/ands
  (curry interpret-bitwise bvand))

(define interpret-eor/eors
  (curry interpret-bitwise bvxor))

(define (interpret-arithmetic proc-Rn proc-imm32 proc-carry cpu S Rn Rd imm12)
  (define-values (d n setflags imm32 carry) (decode cpu S Rn Rd imm12))
  (define-values (result nzcv) (add-with-carry (proc-Rn (cpu-gpr-ref cpu n)) (proc-imm32 imm32) (proc-carry (cpu-pstate.c cpu))))
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


(define-insn (S Rn Rd imm12)
  #:encode (lambda (opc) (list (bv #b0010 4) (bv opc 3) S Rn Rd imm12))
  [(#b000) and/ands-immediate interpret-and/ands]
  [(#b001) eor/eors-immediate interpret-eor/eors]
  [(#b010) sub/subs-immediate interpret-sub/subs]
  [(#b011) rsb/rsbs-immediate interpret-rsb/rsbs]
  [(#b100) add/adds-immediate interpret-add/adds]
  [(#b101) adc/adcs-immediate interpret-adc/adcs]
  [(#b110) sbc/sbcs-immediate interpret-sbc/sbcs]
  [(#b111) rsc/rscs-immediate interpret-rsc/rscs])

(define and-immediate
  (curry and/ands-immediate (bv #b0 1)))

(define ands-immediate
  (curry and/ands-immediate (bv #b1 1)))

(define eor-immediate
  (curry eor/eors-immediate (bv #b0 1)))

(define eors-immediate
  (curry eor/eors-immediate (bv #b1 1)))

(define sub-immediate
  (curry sub/subs-immediate (bv #b0 1)))

(define subs-immediate
  (curry sub/subs-immediate (bv #b1 1)))

(define rsb-immediate
  (curry rsb/rsbs-immediate (bv #b0 1)))

(define rsbs-immediate
  (curry rsb/rsbs-immediate (bv #b1 1)))

(define add-immediate
  (curry add/adds-immediate (bv #b0 1)))

(define adds-immediate
  (curry add/adds-immediate (bv #b1 1)))

(define adc-immediate
  (curry adc/adcs-immediate (bv #b0 1)))

(define adcs-immediate
  (curry adc/adcs-immediate (bv #b1 1)))

(define sbc-immediate
  (curry sbc/sbcs-immediate (bv #b0 1)))

(define sbcs-immediate
  (curry sbc/sbcs-immediate (bv #b1 1)))

(define rsc-immediate
  (curry rsc/rscs-immediate (bv #b0 1)))

(define rscs-immediate
  (curry rsc/rscs-immediate (bv #b1 1)))
