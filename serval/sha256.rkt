#lang rosette

(require rosette/lib/match
         "lib/core.rkt")

(provide (all-defined-out))

(struct sha256 (state count) #:transparent)

(define (sha256-init)
  (define state (list
    (bv #x6a09e667 32)
    (bv #xbb67ae85 32)
    (bv #x3c6ef372 32)
    (bv #xa54ff53a 32)
    (bv #x510e527f 32)
    (bv #x9b05688c 32)
    (bv #x1f83d9ab 32)
    (bv #x5be0cd19 32)))
  (sha256 state (bv 0 32)))

(define (Ch x y z)
  (bvxor z
         (bvand x (bvxor y z))))

(define (Maj x y z)
  (bvor (bvand x y)
        (bvand z (bvor x y))))

(define (rotate-right x n)
  (define w (bv-size n))
  (bvor (bvlshr x n) (bvshl x (bvsub (bv w w) n))))

(define (Sigma0 x)
  (bvxor (rotate-right x (bv 2 32))
         (rotate-right x (bv 13 32))
         (rotate-right x (bv 22 32))))

(define (Sigma1 x)
  (bvxor (rotate-right x (bv 6 32))
         (rotate-right x (bv 11 32))
         (rotate-right x (bv 25 32))))

(define (Gamma0 x)
  (bvxor (rotate-right x (bv 7 32))
         (rotate-right x (bv 18 32))
         (bvlshr x (bv 3 32))))

(define (Gamma1 x)
  (bvxor (rotate-right x (bv 17 32))
         (rotate-right x (bv 19 32))
         (bvlshr x (bv 10 32))))

(define K (vector
  #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
  #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
  #xd807aa98 #x12835b01 #x243185be #x550c7dc3
  #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
  #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
  #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
  #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
  #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
  #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
  #x650a7354 #x766a0abb #x81c2c92e #x92722c85
  #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
  #xd192e819 #xd6990624 #xf40e3585 #x106aa070
  #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
  #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
  #x748f82ee #x78a5636f #x84c87814 #x8cc70208
  #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(define (sha256-transform state data)
  (define (W i)
    (if (< i 16)
        (list->bitvector/be (take (drop data (* i 4)) 4))
        (bvadd (Gamma1 (W (- i 2)))
               (W (- i 7))
               (Gamma0 (W (- i 15)))
               (W (- i 16)))))
  (define (RND state k w)
    (match state
      [(list a b c d e f g h)
       (define t0 (bvadd h (Sigma1 e) (Ch e f g) k w))
       (define t1 (bvadd (Sigma0 a) (Maj a b c)))
       (list (bvadd t0 t1) a b c (bvadd d t0) e f g)]))
  (define (round state n)
    (if (< n 0)
        state
        (RND (round state (- n 1)) (bv (vector-ref K n) 32) (W n))))
  (define s (round state 63))
  (map bvadd state s))


(define (sha256-update ctx data)
  (sha256 (sha256-transform (sha256-state ctx) data)
          (bvadd (sha256-count ctx) (bv 1 32))))

(define (sha256-update-multi ctx data)
  (if (empty? data)
      ctx
      (let-values ([(head tail) (split-at data 64)])
        (sha256-update-multi (sha256-update ctx head) tail))))

(define (sha256-update-last ctx data)
  (define len (length data))
  (define total (bvadd (bvmul (bv 64 32) (sha256-count ctx)) (bv len 32)))
  (define last-data
    (append data
            (list (bv #x80 8))
            (make-list (- (if (< len 56) 55 (+ 63 56)) len) (bv 0 8))
            (bitvector->list/be (zero-extend (bvshl total (bv 3 32)) (bitvector 64)))))
  (sha256-update-multi ctx last-data))

(define (sha256-finish ctx)
  (flatten (map bitvector->list/be (sha256-state ctx))))

(define (sha256-hash data)
  (define len (length data))
  (define-values (n r) (quotient/remainder len 64))
  (define-values (head tail) (split-at-right data r))
  (sha256-finish
    (sha256-update-last
      (sha256-update-multi (sha256-init) head)
      tail)))

; (sha256-hash (list))
; (sha256-hash (make-list 64 (bv (char->integer #\a) 8)))
