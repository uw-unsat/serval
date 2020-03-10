#lang rosette/safe

(provide (all-defined-out))

(struct refcnt (cnt perm perm-inv) #:transparent)

(define (make-havoc-refcnt)
  (define-symbolic* cnt (~> (bitvector 64) (bitvector 64)))
  (define-symbolic* perm (~> (bitvector 64) (bitvector 64) (bitvector 64)))
  (define-symbolic* perm-inv (~> (bitvector 64) (bitvector 64) (bitvector 64)))
  (refcnt cnt perm perm-inv))

(define (init-refcnt)
  (refcnt
    (lambda (own) (bv 0 64))
    (lambda (own idx) idx)
    (lambda (own obj) obj)))

(define (incr-refcnt ref owner object)
  (define old-cnt (refcnt-cnt ref))
  (define old-perm (refcnt-perm ref))
  (define old-perm-inv (refcnt-perm-inv ref))

  (define old-owned (old-perm owner (old-cnt owner)))
  (define old-idx (old-perm-inv owner object))

  (define (new-cnt own)
    (if (bveq own owner)
        (bvadd (bv 1 64) (old-cnt own))
        (old-cnt own)))

  (define (new-perm own idx)
    (cond
      [(&& (bveq own owner) (bveq idx (old-cnt owner))) object]
      [(&& (bveq own owner) (bveq idx old-idx)) old-owned]
      [else (old-perm own idx)]))

  (define (new-perm-inv own obj)
    (cond
      [(&& (bveq own owner) (bveq obj object)) (old-cnt owner)]
      [(&& (bveq own owner) (bveq obj old-owned)) old-idx]
      [else (old-perm-inv own obj)]))

  (refcnt new-cnt new-perm new-perm-inv))

(define (decr-refcnt ref owner object)
  (define old-cnt (refcnt-cnt ref))
  (define old-perm (refcnt-perm ref))
  (define old-perm-inv (refcnt-perm-inv ref))

  (define old-owned (old-perm owner (bvsub (old-cnt owner) (bv 1 64))))
  (define old-idx (old-perm-inv owner object))

  (define (new-cnt own)
    (if (bveq own owner)
        (bvsub (old-cnt own) (bv 1 64))
        (old-cnt own)))

  (define (new-perm own idx)
    (cond
      [(&& (bveq own owner) (bveq idx (bvsub (old-cnt own) (bv 1 64)))) object]
      [(&& (bveq own owner) (bveq idx old-idx)) old-owned]
      [else (old-perm own idx)]))

  (define (new-perm-inv own obj)
    (cond
      [(&& (bveq own owner) (bveq obj object)) (bvsub (old-cnt own) (bv 1 64))]
      [(&& (bveq own owner) (bveq obj old-owned)) old-idx]
      [else (old-perm-inv own obj)]))

  (refcnt new-cnt new-perm new-perm-inv))

(define (refcnt-invariants refcnt owner-valid? object-valid? max-refs owned-by?)
  (define cnt (refcnt-cnt refcnt))
  (define perm (refcnt-perm refcnt))
  (define perm-inv (refcnt-perm-inv refcnt))

  (define-symbolic owner object idx (bitvector 64))

  (&&
    (forall (list owner object)
      (=> (&& (owner-valid? owner) (object-valid? object))
        (bvult (perm-inv owner object) max-refs)))
    (forall (list owner object idx)
      (=> (&& (owner-valid? owner) (object-valid? object) (bvult idx max-refs))
        (&& (bveq idx (perm-inv owner (perm owner idx)))
            (bveq (perm owner (perm-inv owner object)) object))))

    ; The owner is always the first page to allocate and the last one to go
    (forall (list owner)
      (=> (&& (owner-valid? owner) (owned-by? owner owner))
        (&& (bveq (perm-inv owner owner) (bv 0 64))
            (bveq (perm owner (bv 0 64)) owner))))

    (forall (list owner object)
      (=> (&& (owner-valid? owner) (object-valid? object))
        (<=> (owned-by? owner object)
             (bvult (perm-inv owner object) (cnt owner)))))

    (forall (list owner)
      (=> (owner-valid? owner)
        (bvule (cnt owner) max-refs)))
  ))
