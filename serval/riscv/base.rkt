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

; General-purpose registers
(struct gprs
  (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
   x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  #:mutable
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
    (&&
      (equal?-recur (gprs-x1 a) (gprs-x1 b))
      (equal?-recur (gprs-x2 a) (gprs-x2 b))
      (equal?-recur (gprs-x3 a) (gprs-x3 b))
      (equal?-recur (gprs-x4 a) (gprs-x4 b))
      (equal?-recur (gprs-x5 a) (gprs-x5 b))
      (equal?-recur (gprs-x6 a) (gprs-x6 b))
      (equal?-recur (gprs-x7 a) (gprs-x7 b))
      (equal?-recur (gprs-x8 a) (gprs-x8 b))
      (equal?-recur (gprs-x9 a) (gprs-x9 b))
      (equal?-recur (gprs-x10 a) (gprs-x10 b))
      (equal?-recur (gprs-x11 a) (gprs-x11 b))
      (equal?-recur (gprs-x12 a) (gprs-x12 b))
      (equal?-recur (gprs-x13 a) (gprs-x13 b))
      (equal?-recur (gprs-x14 a) (gprs-x14 b))
      (equal?-recur (gprs-x15 a) (gprs-x15 b))
      (equal?-recur (gprs-x16 a) (gprs-x16 b))
      (equal?-recur (gprs-x17 a) (gprs-x17 b))
      (equal?-recur (gprs-x18 a) (gprs-x18 b))
      (equal?-recur (gprs-x19 a) (gprs-x19 b))
      (equal?-recur (gprs-x20 a) (gprs-x20 b))
      (equal?-recur (gprs-x21 a) (gprs-x21 b))
      (equal?-recur (gprs-x22 a) (gprs-x22 b))
      (equal?-recur (gprs-x23 a) (gprs-x23 b))
      (equal?-recur (gprs-x24 a) (gprs-x24 b))
      (equal?-recur (gprs-x25 a) (gprs-x25 b))
      (equal?-recur (gprs-x26 a) (gprs-x26 b))
      (equal?-recur (gprs-x27 a) (gprs-x27 b))
      (equal?-recur (gprs-x28 a) (gprs-x28 b))
      (equal?-recur (gprs-x29 a) (gprs-x29 b))
      (equal?-recur (gprs-x30 a) (gprs-x30 b))
      (equal?-recur (gprs-x31 a) (gprs-x31 b))))
   (define (hash-proc a hash-recur) 1)
   (define (hash2-proc a hash2-recur) 22)])


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
  (define-symbolic* x (bitvector (XLEN)) [31])
  (define gpr-vals (apply gprs x))

  (define csrs (init-csrs))

  (define mregions (core:create-mregions symbols globals))
  (define shims (make-hash))

  ; Reset vector is where PC will be set upon CPU reset
  (define reset-vector (bv #x0000000080000000 (XLEN)))

  (cpu reset-vector gpr-vals csrs mregions shims))


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

; Convert GPR name to integer index.
; Useful for encoding RISC-V instructions.
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
  (define r (cpu-gprs cpu))
  (case gpr
    [(x0 zero) (void)] ; Drop writes to x0
    [(x1 ra) (set-gprs-x1! r val)]
    [(x2 sp) (set-gprs-x2! r val)]
    [(x3 gp) (set-gprs-x3! r val)]
    [(x4 tp) (set-gprs-x4! r val)]
    [(x5 t0) (set-gprs-x5! r val)]
    [(x6 t1) (set-gprs-x6! r val)]
    [(x7 t2) (set-gprs-x7! r val)]
    [(x8 s0 fp) (set-gprs-x8! r val)]
    [(x9 s1) (set-gprs-x9! r val)]
    [(x10 a0) (set-gprs-x10! r val)]
    [(x11 a1) (set-gprs-x11! r val)]
    [(x12 a2) (set-gprs-x12! r val)]
    [(x13 a3) (set-gprs-x13! r val)]
    [(x14 a4) (set-gprs-x14! r val)]
    [(x15 a5) (set-gprs-x15! r val)]
    [(x16 a6) (set-gprs-x16! r val)]
    [(x17 a7) (set-gprs-x17! r val)]
    [(x18 s2) (set-gprs-x18! r val)]
    [(x19 s3) (set-gprs-x19! r val)]
    [(x20 s4) (set-gprs-x20! r val)]
    [(x21 s5) (set-gprs-x21! r val)]
    [(x22 s6) (set-gprs-x22! r val)]
    [(x23 s7) (set-gprs-x23! r val)]
    [(x24 s8) (set-gprs-x24! r val)]
    [(x25 s9) (set-gprs-x25! r val)]
    [(x26 s10) (set-gprs-x26! r val)]
    [(x27 s11) (set-gprs-x27! r val)]
    [(x28 t3) (set-gprs-x28! r val)]
    [(x29 t4) (set-gprs-x29! r val)]
    [(x30 t5) (set-gprs-x30! r val)]
    [(x31 t6) (set-gprs-x31! r val)]
    [else (core:bug-on #t #:msg (format "gpr-set!: unknown gpr ~e" gpr) #:dbg current-pc-debug)]
  ))

(define (gpr-havoc! cpu gpr)
  (define-symbolic* havoc (bitvector (XLEN)))
  (gpr-set! cpu gpr havoc))

(define (havoc-caller-saved! cpu)
  (for ([gpr '(ra t0 t1 t2 a0 a1 a2 a3 a4 a5 a6 a7 t3 t4 t5 t6)])
    (gpr-havoc! cpu gpr)))

(define (gpr-ref cpu gpr)
  (define r (cpu-gprs cpu))
  (case gpr
    [(x0 zero) (bv 0 (XLEN))]
    [(x1 ra) (gprs-x1 r)]
    [(x2 sp) (gprs-x2 r)]
    [(x3 gp) (gprs-x3 r)]
    [(x4 tp) (gprs-x4 r)]
    [(x5 t0) (gprs-x5 r)]
    [(x6 t1) (gprs-x6 r)]
    [(x7 t2) (gprs-x7 r)]
    [(x8 s0 fp) (gprs-x8 r)]
    [(x9 s1) (gprs-x9 r)]
    [(x10 a0) (gprs-x10 r)]
    [(x11 a1) (gprs-x11 r)]
    [(x12 a2) (gprs-x12 r)]
    [(x13 a3) (gprs-x13 r)]
    [(x14 a4) (gprs-x14 r)]
    [(x15 a5) (gprs-x15 r)]
    [(x16 a6) (gprs-x16 r)]
    [(x17 a7) (gprs-x17 r)]
    [(x18 s2) (gprs-x18 r)]
    [(x19 s3) (gprs-x19 r)]
    [(x20 s4) (gprs-x20 r)]
    [(x21 s5) (gprs-x21 r)]
    [(x22 s6) (gprs-x22 r)]
    [(x23 s7) (gprs-x23 r)]
    [(x24 s8) (gprs-x24 r)]
    [(x25 s9) (gprs-x25 r)]
    [(x26 s10) (gprs-x26 r)]
    [(x27 s11) (gprs-x27 r)]
    [(x28 t3) (gprs-x28 r)]
    [(x29 t4) (gprs-x29 r)]
    [(x30 t5) (gprs-x30 r)]
    [(x31 t6) (gprs-x31 r)]
    [else (core:bug-on #t #:msg (format "gpr-ref: unknown gpr ~e" gpr) #:dbg current-pc-debug)]
  ))
