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


; common library

(define (add-with-carry x y carry_in)
  (define N (core:bv-size x))
  (define carry? (bitvector->bool carry_in))
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

(define (extend x N unsigned)
  ((if unsigned zero-extend sign-extend) x (bitvector N)))

(define (highest-set-bit x)
  (define lst (reverse (range (core:bv-size x))))
  ; if bit i is set, return i (otherwise #f)
  (define (pred i) (if (bveq (bit i x) (bv #b1 1)) i #f))
  ; find the highest bit set (#f if x is zero)
  (define pos (ormap pred lst))
  ; return -1 if no bit is set
  (if pos pos -1))

(define (is-zero x)
  (bvzero? x))

(define (is-zero-bit x)
  (if (is-zero x) (bv #b1 1) (bv #b0 1)))

(define (ones n)
  ; Use bv for now, but can switch to for/all if a symbolic n is needed.
  ; Don't use integer->bitvector here.
  (bv -1 n))

(define (replicate x n)
  (apply concat (build-list n (lambda (i) x))))

(define (undefined)
  (core:bug #:msg "UNDEFINED"))

(define (unreachable)
  (core:bug #:msg "UNREACHABLE"))

(define (zeros n)
  ; Use bv for now, but can switch to for/all if a symbolic n is needed.
  ; Don't use integer->bitvector here.
  (bv 0 n))


; aarch64/exceptions/aborts/

(define (sp-alignment-fault cpu)
  (core:bug #:msg "SP ALIGNMENT FAULT"))


; aarch64/functions/memory/

(define (check-sp-alignment cpu)
  (define sp (cpu-sp-ref cpu))
  ; We don't have a model of SA yet, so just be strict.
  (define stack_align_check #t)
  (when (&& stack_align_check (! (core:bvaligned? sp (bv 16 64))))
    (sp-alignment-fault cpu)))

(define (mem-atomic cpu address op value ldacctype stacctype)
  (define offset (bv 0 64))
  (define size (core:bv-size value))
  (define mm (cpu-memmgr cpu))

  ; Ignore ldacctype and stacctype for now
  (core:memmgr-atomic-begin mm)

  (define oldvalue
    (core:memmgr-load mm address offset (bv (quotient size 8) 64)))

  (define newvalue
    (case op
      [(ADD) (bvadd oldvalue value)]
      [(BIC) (bvand oldvalue (bvnot value))]
      [(EOR) (bvxor oldvalue value)]
      [(ORR) (bvor oldvalue value)]
      [(SMAX) (if (bvsgt oldvalue value) oldvalue value)]
      [(SMIN) (if (bvsgt oldvalue value) value oldvalue)]
      [(UMAX) (if (bvugt oldvalue value) oldvalue value)]
      [(UMIN) (if (bvugt oldvalue value) value oldvalue)]
      [(SWP)  value]))

  (core:memmgr-store! mm address offset newvalue (bv (quotient size 8) 64))

  (core:memmgr-atomic-end mm)

  ; Load operations return the old (pre-operation) value
  oldvalue)


; aarch64/instrs/extendreg/

(define (decode-reg-extend op)
  (cond
    [(bveq op (bv #b000 3)) 'UXTB]
    [(bveq op (bv #b001 3)) 'UXTH]
    [(bveq op (bv #b010 3)) 'UXTW]
    [(bveq op (bv #b011 3)) 'UXTX]
    [(bveq op (bv #b100 3)) 'SXTB]
    [(bveq op (bv #b101 3)) 'SXTH]
    [(bveq op (bv #b110 3)) 'SXTW]
    [(bveq op (bv #b111 3)) 'SXTX]))

(define (extend-reg N cpu reg exttype shift)
  (assert (&& (>= shift 0) (<= shift 4)))
  (define val (trunc N (cpu-gpr-ref cpu reg)))
  (define-values (unsigned len)
    (case exttype
      [(SXTB) (values #f 8)]
      [(SXTH) (values #f 16)]
      [(SXTW) (values #f 32)]
      [(SXTX) (values #f 64)]
      [(UXTB) (values #t 8)]
      [(UXTH) (values #t 16)]
      [(UXTW) (values #t 32)]
      [(UXTX) (values #t 64)]))

  ; Note the extended width of the intermediate value and
  ; that sign extension occurs from bit <len+shift-1>, not
  ; from bit <len-1>. This is equivalent to the instruction
  ;   [SU]BFIZ Rtmp, Rreg, #shift, #len
  ; It may also be seen as a sign/zero extend followed by a shift:
  ;   LSL(Extend(val<len-1:0>, N, unsigned), shift);

  (set! len (min len (- N shift)))
  (set! val (trunc len val))
  (unless (zero? shift)
    (set! val (concat val (zeros shift))))
  (extend val N unsigned))


; aarch64/instrs/integer/bitmasks/

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


; aarch64/instrs/integer/shiftreg/

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
