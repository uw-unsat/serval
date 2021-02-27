#lang rosette

(require
  "common.rkt")

(provide
  stmdb
  ldmia)

(define (decode-stmdb W Rn register_list)
  (define n Rn)
  (define registers register_list)
  (define wback (equal? W (bv 1 1)))
  (when (|| (r15? n) (bvzero? (bit-count registers)))
    (unpredictable))
  (values n registers wback))

(define (interpret-stmdb cpu W Rn register_list)
  (define mm (cpu-memmgr cpu))

  (define-values (n registers wback) (decode-stmdb W Rn register_list))

  (define address (bvsub (cpu-gpr-ref cpu Rn)
                         (bvmul (bv 4 32)
                                (zero-extend (bit-count registers) (bitvector 32)))))

  (for ([i (in-range 15)])
    (when (equal? (bit i registers) (bv 1 1))
      (if (&& (equal? (integer->gpr i) n) wback (! (equal? (bv i (type-of registers)) (lowest-set-bit registers))))
          (unpredictable)
          (core:memmgr-store! mm address (bv 0 32) (cpu-gpr-ref cpu (integer->gpr i)) (bv 4 32)))

      (set! address (bvadd address (bv 4 (type-of address))))))

  (when (equal? (bit 15 registers) (bv 1 1))
    (core:memmgr-store! mm address (bv 0 32) (pc-store-value cpu) (bv 4 32)))

  (when wback
    (cpu-gpr-set! cpu Rn
                  (bvsub (cpu-gpr-ref cpu Rn)
                         (bvmul (bv 4 32)
                                (zero-extend (bit-count registers) (bitvector 32)))))))

(define (decode-ldmia W Rn register_list)
  (define n Rn)
  (define registers register_list)
  (define wback (equal? W (bv 1 1)))
  (when (|| (r15? n) (bvzero? (bit-count registers)))
    (unpredictable))
  (values n registers wback))

(define (interpret-ldmia cpu W Rn register_list)
  (define mm (cpu-memmgr cpu))
  (define-values (n registers wback) (decode-ldmia W Rn register_list))

  (define address (cpu-gpr-ref cpu n))

  (for ([i (in-range 15)])
    (when (equal? (bit i registers) (bv 1 1))
      (cpu-gpr-set! cpu (integer->gpr i)
                    (core:memmgr-load mm address (bv 0 32) (bv 4 32)))
      (set! address (bvadd address (bv 4 (type-of address))))))

  (when (equal? (bit 15 registers) (bv 1 1))
    (load-write-pc cpu (core:memmgr-load mm address (bv 0 32) (bv 4 32))))

  (cond
    [(&& wback (equal? (bit 13 registers) (bv 0 1)))
      (cpu-gpr-set! cpu n
        (bvadd (cpu-gpr-ref cpu n)
               (bvmul (bv 4 32) (zero-extend (bit-count registers) (bitvector 32)))))]
    [(&& wback (equal? (bit 13 registers) (bv 1 1)))
      (define-symbolic* UNKNOWN (bitvector 32))
      (cpu-gpr-set! cpu n UNKNOWN)]
    [else (void)]))

(define-insn (W Rn register_list)
  #:encode (lambda (P U op L) (list (bv #b100 3) (bv P 1) (bv U 1) (bv op 1) W (bv L 1) Rn register_list))
  [(#b1 #b0 #b0 #b0) stmdb interpret-stmdb]
  [(#b0 #b1 #b0 #b1) ldmia interpret-ldmia])
