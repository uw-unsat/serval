#lang rosette

(require
  "common.rkt")

(provide
  (rename-out
    [@sbfm sbfm]
    [@bfm  bfm]
    [@ubfm ubfm])
  asr lsl lsr
  sxtb sxth sxtw
  uxtb uxth)


(define (decode sf N immr imms Rn Rd)
  (define d Rd)
  (define n Rn)
  (define datasize (if (bveq sf (bv #b1 1)) 64 32))

  ; sf == '1' && N != '1'
  (when (&& (bveq sf (bv #b1 1))
            (! (bveq N (bv #b1 1))))
    (undefined))

  ; sf == '0' && (N != '0' || immr<5> != '0' || imms<5> != '0')
  (when (&& (bveq sf (bv #b0 1))
            (|| (! (bveq N (bv #b0 1)))
                (! (bveq (bit 5 immr) (bv #b0 1)))
                (! (bveq (bit 5 imms) (bv #b0 1)))))
    (undefined))

  (define R (zero-extend immr (bitvector datasize)))
  (define S (zero-extend imms (bitvector datasize)))
  (define-values (wmask tmask) (decode-bit-masks datasize N imms immr #f))
  (values d n datasize R S wmask tmask))


(define (interpret-sbfm cpu sf N immr imms Rn Rd)
  (define-values (d n datasize R S wmask tmask) (decode sf N immr imms Rn Rd))
  (define src (trunc datasize (cpu-gpr-ref cpu n)))

  ; perform bitfield move on low bits
  (define bot (bvand (bvror src R) wmask))

  ; determine extension bits (sign, zero or dest register)
  (define top (replicate (bv-bit S src) datasize))

  ; combine extension bits and result bits
  (cpu-gpr-set! cpu d (bvor (bvand top (bvnot tmask))
                            (bvand bot tmask))))


(define (interpret-bfm cpu sf N immr imms Rn Rd)
  (define-values (d n datasize R S wmask tmask) (decode sf N immr imms Rn Rd))
  (define dst (trunc datasize (cpu-gpr-ref cpu d)))
  (define src (trunc datasize (cpu-gpr-ref cpu n)))

  ; perform bitfield move on low bits
  (define bot (bvor (bvand dst (bvnot wmask))
                    (bvand (bvror src R) wmask)))

  ; combine extension bits and result bits
  (cpu-gpr-set! cpu d (bvor (bvand dst (bvnot tmask))
                            (bvand bot tmask))))


(define (interpret-ubfm cpu sf N immr imms Rn Rd)
  (define-values (d n datasize R S wmask tmask) (decode sf N immr imms Rn Rd))
  (define src (trunc datasize (cpu-gpr-ref cpu n)))

  ; perform bitfield move on low bits
  (define bot (bvand (bvror src R) wmask))

  ; combine extension bits and result bits
  (cpu-gpr-set! cpu d (bvand bot tmask)))


(define-insn (sf N immr imms Rn Rd)
  #:encode (lambda (opc) (list sf (bv opc 2) (bv #b100110 6) N immr imms Rn Rd))
  [(#b00) sbfm interpret-sbfm]
  [(#b01) bfm  interpret-bfm]
  [(#b10) ubfm interpret-ubfm])


; Use constructors to ensure sf == N.

(define (@sbfm sf immr imms Rn Rd)
  (sbfm sf sf immr imms Rn Rd))

(define (@bfm sf immr imms Rn Rd)
  (bfm sf sf immr imms Rn Rd))

(define (@ubfm sf immr imms Rn Rd)
  (ubfm sf sf immr imms Rn Rd))


; Aliases for shift (immediate) instructions.

(define (asr sf shift Rn Rd)
  (define imms
    (cond
      [(bveq sf (bv #b0 1)) (bv 31 6)]
      [(bveq sf (bv #b1 1)) (bv 63 6)]))
  (@sbfm sf shift imms Rn Rd))

(define (lsl sf shift Rn Rd)
  (define-values (immr imms)
    (cond
      [(bveq sf (bv #b0 1))
       (values (bvand (bvneg shift) (bv 31 6)) (bvsub (bv 31 6) shift))]
      [(bveq sf (bv #b1 1))
       (values (bvand (bvneg shift) (bv 63 6)) (bvsub (bv 63 6) shift))]))
  (@ubfm sf immr imms Rn Rd))

(define (lsr sf shift Rn Rd)
  (define imms
    (cond
      [(bveq sf (bv #b0 1)) (bv 31 6)]
      [(bveq sf (bv #b1 1)) (bv 63 6)]))
  (@ubfm sf shift imms Rn Rd))


; Aliases for sign-extend and zero-extend instructions.

(define (sxtb sf Rn Rd)
  (@sbfm sf (bv 0 6) (bv 7 6) Rn Rd))

(define (sxth sf Rn Rd)
  (@sbfm sf (bv 0 6) (bv 15 6) Rn Rd))

(define (sxtw Rn Rd)
  (@sbfm (bv #b1 1) (bv 0 6) (bv 31 6) Rn Rd))

(define (uxtb Rn Rd)
  (@ubfm (bv #b0 1) (bv 0 6) (bv 7 6) Rn Rd))

(define (uxth Rn Rd)
  (@ubfm (bv #b0 1) (bv 0 6) (bv 15 6) Rn Rd))
