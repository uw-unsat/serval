#lang rosette

(require
  "define.rkt"
  "../base.rkt"
  (prefix-in core: "../../lib/core.rkt"))

(provide
  (all-defined-out)
  (all-from-out
    "define.rkt"
    "../base.rkt"
    "../../lib/core.rkt"))


(define (r15? r)
  (equal? r (integer->gpr 15)))

(define (see s)
  (assert #f s))

(define (unpredictable)
  (core:bug #:msg "UNPREDICTABLE"))

(define (undefined)
  (core:bug #:msg "UNDEFINED"))

(define (bit-count val)
  (apply bvadd
    (for/list ([b (bitvector->bits val)])
      (zero-extend b (type-of val)))))

(define (lowest-set-bit val)
  (define N (bitvector-size (type-of val)))
  (letrec ([loop (lambda (i)
    (cond
      [(>= i N) (bv N N)]
      [(bitvector->bool (bit i val)) (bv i N)]
      [else (loop (+ i 1))]))])
    (loop 0)))

; aarch32/functions/common/

(define (a32-expand-imm_c imm12 carry_in)
  (define unrotated_value (zero-extend (extract 7 0 imm12) (bitvector 32)))
  (define amount (bvshl (zero-extend (extract 11 8 imm12) (bitvector 32)) (bv 1 32)))
  (define-values (imm32 carry_out) (shift_c unrotated_value 'ROR amount carry_in))
  (values imm32 carry_out))

(define (decode-imm-shift srtype imm5)
  (define result
    (cond
      [(bveq srtype (bv #b00 2))
       (list 'LSL (zero-extend imm5 (bitvector 32)))]
      [(bveq srtype (bv #b01 2))
       (list 'LSR (if (bveq imm5 (bv #b00000 5)) (bv 32 32) (zero-extend imm5 (bitvector 32))))]
      [(bveq srtype (bv #b10 2))
       (list 'ASR (if (bveq imm5 (bv #b00000 5)) (bv 32 32) (zero-extend imm5 (bitvector 32))))]
      [(bveq srtype (bv #b11 2))
       (if (bveq imm5 (bv #b00000 5))
           (list 'RRX (bv 1 32))
           (list 'ROR (zero-extend imm5 (bitvector 32))))]))
  (apply values result))

(define (decode-reg-shift srtype)
  (cond
    [(bveq srtype (bv #b00 2)) 'LSL]
    [(bveq srtype (bv #b01 2)) 'LSR]
    [(bveq srtype (bv #b10 2)) 'ASR]
    [(bveq srtype (bv #b11 2)) 'ROR]))

(define (rrx_c x carry_in)
  (define n (core:bv-size x))
  (define result (concat carry_in (extract (sub1 n) 1 x)))
  (define carry_out (lsb x))
  (list result carry_out))

(define (shift value srtype amount carry_in)
  (define-values (result _) (shift_c value srtype amount carry_in))
  result)

(define (shift_c value srtype amount carry_in)
  (assert (! (&& (equal? srtype 'RRX) (! (bveq amount (bv 1 32))))))
  (define result
    (if (bvzero? amount)
        (list value carry_in)
        (case srtype
          [(LSL) (lsl_c value amount)]
          [(LSR) (lsr_c value amount)]
          [(ASR) (asr_c value amount)]
          [(ROR) (ror_c value amount)]
          [(RRX) (rrx_c value carry_in)])))
  (apply values result))


; aarch32/functions/registers/

(define (alu-exception-return cpu address)
  (undefined))

(define (alu-write-pc cpu address)
  (bx-write-pc cpu address 'INDIR))

(define (branch-write-pc cpu address branch_type)
  ; A symbolic optimization may be needed to remove the address masking.
  (set! address (concat (extract 31 2 address) (bv #b00 2)))
  (branch-to cpu address branch_type))

(define (bx-write-pc cpu address branch_type)
  (cond
    [(bveq (bit 0 address) (bv #b1 1))
     ; SelectInstrSet(InstrSet_T32);
     (set! address (concat (extract 31 1 address) (bv #b0 1)))]
    [else
     ; SelectInstrSet(InstrSet_A32);
     ; For branches to an unaligned PC counter in A32 state, the processor takes the branch
     ; and does one of:
     ; * Forces the address to be aligned
     ; * Leaves the PC unaligned, meaning the target generates a PC Alignment fault.
     ; NB: doing nothing for now
     (void)])
  (branch-to cpu address branch_type))

(define (load-write-pc cpu address)
  (bx-write-pc cpu address 'INDIR))

(define (pc-store-value cpu)
  (bvadd (cpu-pc cpu) (bv 8 32)))


; shared/functions/common/

(define (asr_c x shift)
  (assert (! (bvzero? shift)))
  (define result (bvashr x shift))
  (define carry_out (lsb (bvashr x (bvsub1 shift))))
  (list result carry_out))

(define (is-zero x)
  (bvzero? x))

(define (is-zero-bit x)
  (if (is-zero x) (bv #b1 1) (bv #b0 1)))

(define (lsl_c x shift)
  (assert (! (bvzero? shift)))
  (define result (bvshl x shift))
  (define carry_out (msb (bvshl x (bvsub1 shift))))
  (list result carry_out))

(define (lsr_c x shift)
  (assert (! (bvzero? shift)))
  (define result (bvlshr x shift))
  (define carry_out (lsb (bvlshr x (bvsub1 shift))))
  (list result carry_out))

(define (ror_c x shift)
  (define result (bvror x shift))
  (define carry_out (msb result))
  (list result carry_out))


; shared/functions/integer/

(define (add-with-carry x y carry_in)
  (define N (core:bv-size x))
  (define carry? (bitvector->bool carry_in))
  (define result (bvadd x y (if carry? (bv #b1 N) (bv #b0 N))))
  (define n (bit (sub1 N) result))
  (define z (if (is-zero result) (bv #b1 1) (bv #b0 1)))
  (define c (if (core:bvuadd-overflow? x y carry?) (bv #b1 1) (bv #b0 1)))
  (define v (if (core:bvsadd-overflow? x y carry?) (bv #b1 1) (bv #b0 1)))
  (values result (list n z c v)))


; shared/functions/memory/

(define (big-endian cpu)
  #f)


; shared/functions/registers/

(define (branch-to cpu target branch_type)
  (cpu-pc-set! cpu target)
  #f)
