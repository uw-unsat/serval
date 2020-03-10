#lang rosette/safe

(require
  "base.rkt"
  "../lib/core.rkt"
  (only-in racket/base for/list)
  (only-in racket/list range)
  (only-in racket/match match)
)

(provide pmp-privs)

(define PMPCFG_A_OFF 0)
(define PMPCFG_A_NAPOT 24)
(define PMPCFG_A_TOR 8)
(define PMPCFG_R 1)
(define PMPCFG_W 2)
(define PMPCFG_X 4)
(define PMPCFG_RWX 7)

(define (pmp-privs cpu ptr size)
  (define pmpcfg0 (csr-ref cpu 'pmpcfg0))
  (define pmpcfg2 (csr-ref cpu 'pmpcfg2))

  (define addrs (map (lambda (r) (csr-ref cpu r))
                     '(pmpaddr0 pmpaddr1 pmpaddr2 pmpaddr3
                       pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7
                       pmpaddr8 pmpaddr9 pmpaddr10 pmpaddr11
                       pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15)))
  (define configs
    (list (extract 7 0 pmpcfg0)
          (extract 15 8 pmpcfg0)
          (extract 23 16 pmpcfg0)
          (extract 31 24 pmpcfg0)
          (extract 39 32 pmpcfg0)
          (extract 47 40 pmpcfg0)
          (extract 55 48 pmpcfg0)
          (extract 63 56 pmpcfg0)
          (extract 7 0 pmpcfg2)
          (extract 15 8 pmpcfg2)
          (extract 23 16 pmpcfg2)
          (extract 31 24 pmpcfg2)
          (extract 39 32 pmpcfg2)
          (extract 47 40 pmpcfg2)
          (extract 55 48 pmpcfg2)
          (extract 63 56 pmpcfg2)))

  (define (pmp-privs prev-addr any? cfgs addrs)
    (match (cons cfgs addrs)
      [(cons (cons cfg cfgs-rest) (cons addr addrs-rest))

        (define cfg-a (bvand cfg (bv PMPCFG_A_NAPOT 8)))
        (define cfg-perm (bvand cfg (bv PMPCFG_RWX 8)))
        (define this-addr (bvshl addr (bv 2 64)))

        (cond
          [(bveq cfg-a (bv PMPCFG_A_OFF 8))
            (pmp-privs this-addr any? cfgs-rest addrs-rest)]
          [(bveq cfg-a (bv PMPCFG_A_TOR 8))

            (define overlaps (&& (bvugt (bvadd ptr size) prev-addr)
                                 (bvult ptr this-addr)))
            (define contains (&& (bvuge ptr prev-addr)
                                 (bvule (bvadd ptr size) this-addr)))
            (if overlaps
              (if contains cfg-perm (bv 0 8))
              (pmp-privs this-addr #t cfgs-rest addrs-rest))]
          [else (assert #f)])]
      [(cons null null) (if (! any?) (bv PMPCFG_RWX 8) (bv 0 8))]
      [_ (assert #f)]))

  (pmp-privs (bv 0 64) #f configs addrs))
