#lang rosette

(require
  (prefix-in core: "../lib/core.rkt")
  rosette/base/core/polymorphic)

(provide (all-defined-out))

(define XLEN
  (make-parameter 64
    (lambda (x)
      (when (! (|| (= x 32) (= x 64)))
        (error "Bad XLEN value"))
      x)))

; Allow a default size to be set for debugging code
(define riscv-default-size (make-parameter #f))

(define current-pc-debug (bv 0 (XLEN)))

(define (set-current-pc-debug! v)
  (set! current-pc-debug v))

(struct csrs
  ( ; Supervisor trap setup
    sedeleg sideleg stvec scounteren
    ; Supervisor trap handling
    sscratch sepc scause stval
    ; Supervisor protection and translation
    satp
    ; Machine trap setup
    mstatus misa medeleg mideleg mie mtvec mcounteren
    ; Machine trap handling
    mscratch mepc mcause mtval mip
    ; Machine protection and translation
    pmpcfg0 pmpcfg2
    pmpaddr0 pmpaddr1 pmpaddr2 pmpaddr3
    pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7
    pmpaddr8 pmpaddr9 pmpaddr10 pmpaddr11
    pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15
    ; Machine Counter/Timers
    mcycle minstret)
  #:mutable #:transparent)

(define (init-csrs)
  (define-symbolic*
   sedeleg sideleg stvec scounteren sscratch sepc scause stval satp
    mstatus misa medeleg mideleg mie mtvec mcounteren mscratch mepc
    mcause mtval mip pmpcfg0 pmpcfg2 pmpaddr0 pmpaddr1 pmpaddr2 pmpaddr3 pmpaddr4 pmpaddr5 pmpaddr6
    pmpaddr7 pmpaddr8 pmpaddr9 pmpaddr10 pmpaddr11 pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15 mcycle
    minstret (bitvector (XLEN)))
  (csrs
    sedeleg sideleg stvec scounteren sscratch sepc scause stval satp
    mstatus misa medeleg mideleg mie mtvec mcounteren mscratch mepc mcause mtval mip pmpcfg0 pmpcfg2
    pmpaddr0 pmpaddr1 pmpaddr2 pmpaddr3 pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7 pmpaddr8 pmpaddr9
    pmpaddr10 pmpaddr11 pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15 mcycle minstret))

(struct cpu (pc gprs csrs mregions shims) #:mutable #:transparent)

(define (cpu-copy c)
  (struct-copy cpu c
    [gprs (vector-copy (cpu-gprs c))]
    [csrs (struct-copy csrs (cpu-csrs c))]
    [mregions (map core:mregion-copy (cpu-mregions c))]))

(define (cpu-add-shim! cpu addr shim)
  (core:bug-on (! (equal? (pc) #t)) #:msg "cpu-add-shim!: path condition not #t" #:dbg current-pc-debug)
  (core:bug-on (! (equal? (asserts) null)) #:msg "cpu-add-shim!: asserts not empty" #:dbg current-pc-debug)
  (hash-set! (cpu-shims cpu) addr shim))

(define (init-cpu [symbols null] [globals null])
  (define-symbolic* x (bitvector (XLEN)) [32])
  (define gprs (apply vector x))

  (define csrs (init-csrs))

  (define mregions (core:create-mregions symbols globals))
  (define shims (make-hash))

  ; Reset vector is where PC will be set upon CPU reset
  (define reset-vector (bv #x0000000080000000 (XLEN)))

  (cpu reset-vector gprs csrs mregions shims))


(define (cpu-equal? cpu1 cpu2)
  (&& (bveq (cpu-pc cpu1) (cpu-pc cpu2))
      (equal? (cpu-gprs cpu1) (cpu-gprs cpu2))
      (equal? (cpu-csrs cpu1) (cpu-csrs cpu2))))

(define sstatus-mask
  (bv #b1000000000000000000000000000001100000000000011011110000100110011 (XLEN)))

(define sip-mask
  (bv #b1100110011 (XLEN)))

(define sie-mask
  (bv #b1100110011 (XLEN)))

(define (csr-set! cpu csr val)
  (core:bug-on (term? csr) #:msg (format "csr-set!: symbolic csr ~e" csr) #:dbg current-pc-debug)
  (case csr
    [(sstatus) (set-csrs-mstatus! (cpu-csrs cpu) (bvand val sstatus-mask))]
    [(sedeleg) (set-csrs-sedeleg! (cpu-csrs cpu) val)]
    [(sideleg) (set-csrs-sideleg! (cpu-csrs cpu) val)]
    [(sie) (set-csrs-mie! (cpu-csrs cpu) (bvand sie-mask val))]
    [(stvec) (set-csrs-stvec! (cpu-csrs cpu) val)]
    [(scounteren) (set-csrs-scounteren! (cpu-csrs cpu) val)]

    [(sscratch) (set-csrs-sscratch! (cpu-csrs cpu) val)]
    [(sepc) (set-csrs-sepc! (cpu-csrs cpu) val)]
    [(scause) (set-csrs-scause! (cpu-csrs cpu) val)]
    [(stval) (set-csrs-stval! (cpu-csrs cpu) val)]
    [(sip) (set-csrs-mip! (cpu-csrs cpu) (bvand val sip-mask))]
    [(satp) (set-csrs-satp! (cpu-csrs cpu) val)]

    [(mstatus) (set-csrs-mstatus! (cpu-csrs cpu) val)]
    [(misa) (set-csrs-misa! (cpu-csrs cpu) val)]
    [(medeleg) (set-csrs-medeleg! (cpu-csrs cpu) val)]
    [(mideleg) (set-csrs-mideleg! (cpu-csrs cpu) val)]
    [(mie) (set-csrs-mie! (cpu-csrs cpu) val)]
    [(mtvec) (set-csrs-mtvec! (cpu-csrs cpu) val)]
    [(mcounteren) (set-csrs-mcounteren! (cpu-csrs cpu) val)]

    [(mscratch) (set-csrs-mscratch! (cpu-csrs cpu) val)]
    [(mepc) (set-csrs-mepc! (cpu-csrs cpu) val)]
    [(mcause) (set-csrs-mcause! (cpu-csrs cpu) val)]
    [(mtval) (set-csrs-mtval! (cpu-csrs cpu) val)]
    [(mip) (set-csrs-mip! (cpu-csrs cpu) val)]

    [(pmpcfg0) (set-csrs-pmpcfg0! (cpu-csrs cpu) val)]
    [(pmpcfg2) (set-csrs-pmpcfg2! (cpu-csrs cpu) val)]

    [(pmpaddr0) (set-csrs-pmpaddr0! (cpu-csrs cpu) val)]
    [(pmpaddr1) (set-csrs-pmpaddr1! (cpu-csrs cpu) val)]
    [(pmpaddr2) (set-csrs-pmpaddr2! (cpu-csrs cpu) val)]
    [(pmpaddr3) (set-csrs-pmpaddr3! (cpu-csrs cpu) val)]
    [(pmpaddr4) (set-csrs-pmpaddr4! (cpu-csrs cpu) val)]
    [(pmpaddr5) (set-csrs-pmpaddr5! (cpu-csrs cpu) val)]
    [(pmpaddr6) (set-csrs-pmpaddr6! (cpu-csrs cpu) val)]
    [(pmpaddr7) (set-csrs-pmpaddr7! (cpu-csrs cpu) val)]
    [(pmpaddr8) (set-csrs-pmpaddr8! (cpu-csrs cpu) val)]
    [(pmpaddr9) (set-csrs-pmpaddr9! (cpu-csrs cpu) val)]
    [(pmpaddr10) (set-csrs-pmpaddr10! (cpu-csrs cpu) val)]
    [(pmpaddr11) (set-csrs-pmpaddr11! (cpu-csrs cpu) val)]
    [(pmpaddr12) (set-csrs-pmpaddr12! (cpu-csrs cpu) val)]
    [(pmpaddr13) (set-csrs-pmpaddr13! (cpu-csrs cpu) val)]
    [(pmpaddr14) (set-csrs-pmpaddr14! (cpu-csrs cpu) val)]
    [(pmpaddr15) (set-csrs-pmpaddr15! (cpu-csrs cpu) val)]

    [else (core:bug-on #t #:msg (format "csr-set!: unknown csr ~e" csr) #:dbg current-pc-debug)]))

(define (csr-ref cpu csr)
  (core:bug-on (term? csr) #:msg (format "csr-ref: symbolic csr ~e" csr) #:dbg current-pc-debug)
  (case csr
    [(sstatus) (bvand sstatus-mask (csrs-mstatus (cpu-csrs cpu)))]
    [(sedeleg) (csrs-sedeleg (cpu-csrs cpu))]
    [(sideleg) (csrs-sideleg (cpu-csrs cpu))]
    [(sie) (bvand sie-mask (csrs-mie (cpu-csrs cpu)))]
    [(stvec) (csrs-stvec (cpu-csrs cpu))]
    [(scounteren) (csrs-scounteren (cpu-csrs cpu))]

    [(sscratch) (csrs-sscratch (cpu-csrs cpu))]
    [(sepc) (csrs-sepc (cpu-csrs cpu))]
    [(scause) (csrs-scause (cpu-csrs cpu))]
    [(stval) (csrs-stval (cpu-csrs cpu))]
    [(sip) (bvand sip-mask (csrs-mip (cpu-csrs cpu)))]
    [(satp) (csrs-satp (cpu-csrs cpu))]

    [(marchid) (bv -1 (XLEN))]
    [(mimpid) (bv 0 (XLEN))]
    [(mvendorid) (bv 0 (XLEN))]
    [(mhartid) (bv 1 (XLEN))]
    [(mstatus) (csrs-mstatus (cpu-csrs cpu))]
    [(misa) (csrs-misa (cpu-csrs cpu))]
    [(medeleg) (csrs-medeleg (cpu-csrs cpu))]
    [(mideleg) (csrs-mideleg (cpu-csrs cpu))]
    [(mie) (csrs-mie (cpu-csrs cpu))]
    [(mtvec) (csrs-mtvec (cpu-csrs cpu))]
    [(mcounteren) (csrs-mcounteren (cpu-csrs cpu))]

    [(mscratch) (csrs-mscratch (cpu-csrs cpu))]
    [(mepc) (csrs-mepc (cpu-csrs cpu))]
    [(mcause) (csrs-mcause (cpu-csrs cpu))]
    [(mtval) (csrs-mtval (cpu-csrs cpu))]
    [(mip) (csrs-mip (cpu-csrs cpu))]

    [(pmpcfg0) (csrs-pmpcfg0 (cpu-csrs cpu))]
    [(pmpcfg2) (csrs-pmpcfg2 (cpu-csrs cpu))]

    [(pmpaddr0) (csrs-pmpaddr0 (cpu-csrs cpu))]
    [(pmpaddr1) (csrs-pmpaddr1 (cpu-csrs cpu))]
    [(pmpaddr2) (csrs-pmpaddr2 (cpu-csrs cpu))]
    [(pmpaddr3) (csrs-pmpaddr3 (cpu-csrs cpu))]
    [(pmpaddr4) (csrs-pmpaddr4 (cpu-csrs cpu))]
    [(pmpaddr5) (csrs-pmpaddr5 (cpu-csrs cpu))]
    [(pmpaddr6) (csrs-pmpaddr6 (cpu-csrs cpu))]
    [(pmpaddr7) (csrs-pmpaddr7 (cpu-csrs cpu))]
    [(pmpaddr8) (csrs-pmpaddr8 (cpu-csrs cpu))]
    [(pmpaddr9) (csrs-pmpaddr9 (cpu-csrs cpu))]
    [(pmpaddr10) (csrs-pmpaddr10 (cpu-csrs cpu))]
    [(pmpaddr11) (csrs-pmpaddr11 (cpu-csrs cpu))]
    [(pmpaddr12) (csrs-pmpaddr12 (cpu-csrs cpu))]
    [(pmpaddr13) (csrs-pmpaddr13 (cpu-csrs cpu))]
    [(pmpaddr14) (csrs-pmpaddr14 (cpu-csrs cpu))]
    [(pmpaddr15) (csrs-pmpaddr15 (cpu-csrs cpu))]

    [else (core:bug-on #t #:msg (format "csr-ref: unknown csr ~e" csr) #:dbg current-pc-debug)]))

(define (gpr->idx gpr)
  (case gpr
    [(zero x0) 0]
    [(ra x1) 1]
    [(sp x2) 2]
    [(gp x3) 3]
    [(tp x4) 4]
    [(t0 x5) 5]
    [(t1 x6) 6]
    [(t2 x7) 7]
    [(s0 fp x8) 8]
    [(s1 x9) 9]
    [(a0 x10) 10]
    [(a1 x11) 11]
    [(a2 x12) 12]
    [(a3 x13) 13]
    [(a4 x14) 14]
    [(a5 x15) 15]
    [(a6 x16) 16]
    [(a7 x17) 17]
    [(s2 x18) 18]
    [(s3 x19) 19]
    [(s4 x20) 20]
    [(s5 x21) 21]
    [(s6 x22) 22]
    [(s7 x23) 23]
    [(s8 x24) 24]
    [(s9 x25) 25]
    [(s10 x26) 26]
    [(s11 x27) 27]
    [(t3 x28) 28]
    [(t4 x29) 29]
    [(t5 x30) 30]
    [(t6 x31) 31]
    [else
      (if (and (integer? gpr) (&& (>= gpr 0) (< gpr 32)))
          gpr
          (core:bug-on #t #:dbg current-pc-debug #:msg (format "No such GPR ~e\n" gpr)))]))

(define (gpr-set! cpu gpr val)
  (core:bug-on (not (bv? val)) #:msg (format "gpr-set!: not a bitvector: ~e" val) #:dbg current-pc-debug)
  (when (! (= (gpr->idx gpr) 0))
    (vector-set! (cpu-gprs cpu) (gpr->idx gpr) val)))

(define (gpr-havoc! cpu gpr)
  (define-symbolic* havoc (bitvector (XLEN)))
  (gpr-set! cpu gpr havoc))

(define (havoc-caller-saved! cpu)
  (for ([gpr '(ra t0 t1 t2 a0 a1 a2 a3 a4 a5 a6 a7 t3 t4 t5 t6)])
    (gpr-havoc! cpu gpr)))

(define (gpr-ref cpu gpr)
  (if (= 0 (gpr->idx gpr))
      (bv 0 (XLEN))
      (vector-ref (cpu-gprs cpu) (gpr->idx gpr))))
