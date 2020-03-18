#lang rosette

(require
  "define.rkt"
  "../base.rkt"
  (prefix-in core: "../../lib/core.rkt"))

(provide
  (all-defined-out)
  (all-from-out
    "define.rkt"
    "../base.rkt"))


; common library

(define (add-with-carry x y carry_in)
  (define N (core:bv-size x))
  (define carry? (core:bitvector->bool carry_in))
  (define result (bvadd x y (if carry? (bv #b1 N) (bv #b0 N))))
  (define n (bit (sub1 N) result))
  (define z (if (is-zero result) (bv #b1 1) (bv #b0 1)))
  (define c (if (core:bvuadd-overflow? x y carry?) (bv #b1 1) (bv #b0 1)))
  (define v (if (core:bvsadd-overflow? x y carry?) (bv #b1 1) (bv #b0 1)))
  (values result (nzcv n z c v)))

; return #f to skip the default behavior of bumping the PC by 4
(define (branch-to cpu target)
  (cpu-pc-set! cpu target)
  #f)

(define (bvror x shift)
  (define n (let ([N (core:bv-size shift)]) (bv N N)))
  (if (core:bvzero? shift)
      x
      (let ([m (bvurem shift n)])
        (bvor (bvlshr x m) (bvshl x (bvsub n m))))))

(define (condition-holds cpu cc)
  ; Evaluate base condition.
  (define base (extract 3 1 cc))
  (define result
    (cond
      [(bveq base (bv #b000 3))
       (bveq (cpu-pstate.z cpu) (bv #b1 1))]
      [(bveq base (bv #b001 3))
       (bveq (cpu-pstate.c cpu) (bv #b1 1))]
      [(bveq base (bv #b010 3))
       (bveq (cpu-pstate.n cpu) (bv #b1 1))]
      [(bveq base (bv #b011 3))
       (bveq (cpu-pstate.v cpu) (bv #b1 1))]
      [(bveq base (bv #b100 3))
       (&& (bveq (cpu-pstate.c cpu) (bv #b1 1))
           (bveq (cpu-pstate.z cpu) (bv #b0 1)))]
      [(bveq base (bv #b101 3))
       (bveq (cpu-pstate.n cpu) (cpu-pstate.v cpu))]
      [(bveq base (bv #b110 3))
       (&& (bveq (cpu-pstate.n cpu) (cpu-pstate.v cpu))
           (bveq (cpu-pstate.z cpu) (bv #b0 1)))]
      [(bveq base (bv #b111 3))
       #t]))

  ; Condition flag values in the set '111x' indicate always true.
  ; Otherwise, invert condition if necessary.
  (when (&& (bveq (bit 0 cc) (bv #b1 1))
            (! (bveq cc (bv #b1111 4))))
    (set! result (! result)))

  result)

(define (highest-set-bit x)
  (define lst (reverse (range (core:bv-size x))))
  ; if bit i is set, return i (otherwise #f)
  (define (pred i) (if (bveq (bit i x) (bv #b1 1)) i #f))
  ; find the highest bit set (#f if x is zero)
  (define pos (ormap pred lst))
  ; return -1 if no bit is set
  (if pos pos -1))

(define (is-zero x)
  (core:bvzero? x))

(define (is-zero-bit x)
  (if (is-zero x) (bv #b1 1) (bv #b0 1)))

(define (ones n)
  ; Use bv for now, but can switch to for/all if a symbolic n is needed.
  ; Don't use integer->bitvector here.
  (bv -1 n))

(define (replicate x n)
  (apply concat (build-list n (lambda (i) x))))

(define (undefined)
  (assert #f "UNDEFINED"))

(define (unreachable)
  (assert #f "UNREACHABLE"))

(define (zeros n)
  ; Use bv for now, but can switch to for/all if a symbolic n is needed.
  ; Don't use integer->bitvector here.
  (bv 0 n))


; arm64 library

; immN: (bitvector 1)
; immr, immr: (bitvector 6)
; immediate: boolean?
(define (decode-bit-masks M immN imms immr immediate)
  ; Compute log2 of element size.
  ; 2^len must be in range [2, M]
  (define len (highest-set-bit (concat immN (bvnot imms))))

  ; Since len is integer, split over all possible concrete values (up to 6).
  (define lst (for/all ([len len #:exhaustive])
    (assert (integer? len))
    (when (< len 1)
      (undefined))
    (assert (>= M (arithmetic-shift 1 len)))

    ; Determine S, R and S - R parameters.
    (define levels (zero-extend (ones len) (bitvector 6)))

    ; For logical immediates an all-ones value of S is reserved
    ; since it would generate a useless all-ones result (many times).
    (when (&& immediate (bveq (bvand imms levels) levels))
      (undefined))

    ; S = UInt(imms AND levels);
    ; R = UInt(immr AND levels);
    ; diff = S - R;
    ;
    ; To avoid integers, represent S and R using (bitvector len).
    ; Use extract here instead of masking levels.
    (define S (trunc len imms))
    (define R (trunc len immr))
    (define diff (bvsub S R))

    ; esize = 2^len
    (define esize (arithmetic-shift 1 len))
    ; No need for truncation as diff is already of (bitvector len).
    (define d diff)

    ; welem = ZeroExtend(Ones(S + 1), esize);
    ; telem = ZeroExtend(Ones(d + 1), esize);
    ;
    ; To avoid integers, start from all-one values and right-shift instead:
    ; welem: ones(esize) >> (esize - 1 - S)
    ; telem: ones(esize) >> (esize - 1 - d)
    ;
    ; Note that both S and d are of (bitvector len), they need to be zero-extended.
    (define welem (bvlshr (ones esize) (bvsub (bv (sub1 esize) esize) (zero-extend S (bitvector esize)))))
    (define telem (bvlshr (ones esize) (bvsub (bv (sub1 esize) esize) (zero-extend d (bitvector esize)))))

    (define wmask (replicate (bvror welem (zero-extend R (bitvector esize))) (quotient M esize)))
    (define tmask (replicate telem (quotient M esize)))
    (list wmask tmask)))

  ; As values is not lifted by Rosette, use list inside for/all and
  ; values after merging.
  (apply values lst))

(define (decode-shift op)
  (cond
    [(bveq op (bv #b00 2)) 'LSL]
    [(bveq op (bv #b01 2)) 'LSR]
    [(bveq op (bv #b10 2)) 'ASR]
    [(bveq op (bv #b11 2)) 'ROR]))

; avoid integers; assume amount is a bitvector and use its size for shifting
(define (shift-reg cpu reg type amount)
  (define datasize (core:bv-size amount))
  (define result (trunc datasize (cpu-gpr-ref cpu reg)))
  (case type
    [(LSL) (bvshl result amount)]
    [(LSR) (bvlshr result amount)]
    [(ASR) (bvashr result amount)]
    [(ROR) (bvror result amount)]))
