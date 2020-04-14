#lang rosette

(require
  "common.rkt")

(provide
  sxtb16 sxtb sxth
  uxtb16 uxtb uxth)


(define (decode Rd rotate Rm)
  (define d Rd)
  (define m Rm)
  (define rotation (zero-extend (concat rotate (bv #b000 3)) (bitvector 32)))
  (when (|| (r15? d) (r15? m))
    (unpredictable))
  (values d m rotation))

(define (interpret-sxtb16 cpu Rd rotate Rm)
  (define-values (d m rotation) (decode Rd rotate Rm))
  (define rotated (bvror (cpu-gpr-ref cpu m) rotation))
  (cpu-gpr-set! cpu d
    (concat (sign-extend (extract 23 16 rotated) (bitvector 16))
            (sign-extend (extract 7 0 rotated) (bitvector 16)))))

(define (interpret-sxtb cpu Rd rotate Rm)
  (define-values (d m rotation) (decode Rd rotate Rm))
  (define rotated (bvror (cpu-gpr-ref cpu m) rotation))
  (cpu-gpr-set! cpu d (sign-extend (extract 7 0 rotated) (bitvector 32))))

(define (interpret-sxth cpu Rd rotate Rm)
  (define-values (d m rotation) (decode Rd rotate Rm))
  (define rotated (bvror (cpu-gpr-ref cpu m) rotation))
  (cpu-gpr-set! cpu d (sign-extend (extract 15 0 rotated) (bitvector 32))))

(define (interpret-uxtb16 cpu Rd rotate Rm)
  (define-values (d m rotation) (decode Rd rotate Rm))
  (define rotated (bvror (cpu-gpr-ref cpu m) rotation))
  (cpu-gpr-set! cpu d
    (concat (zero-extend (extract 23 16 rotated) (bitvector 16))
            (zero-extend (extract 7 0 rotated) (bitvector 16)))))

(define (interpret-uxtb cpu Rd rotate Rm)
  (define-values (d m rotation) (decode Rd rotate Rm))
  (define rotated (bvror (cpu-gpr-ref cpu m) rotation))
  (cpu-gpr-set! cpu d (zero-extend (extract 7 0 rotated) (bitvector 32))))

(define (interpret-uxth cpu Rd rotate Rm)
  (define-values (d m rotation) (decode Rd rotate Rm))
  (define rotated (bvror (cpu-gpr-ref cpu m) rotation))
  (cpu-gpr-set! cpu d (zero-extend (extract 15 0 rotated) (bitvector 32))))

(define-insn (Rd rotate Rm)
  #:encode (lambda (U op) (list (bv #b01101 5) (bv U 1) (bv op 2) (bv #b1111 4) Rd rotate (bv #b00 2) (bv #b0111 4) Rm))
  [(#b0 #b00) sxtb16 interpret-sxtb16]
  [(#b0 #b10) sxtb   interpret-sxtb]
  [(#b0 #b11) sxth   interpret-sxth]
  [(#b1 #b00) uxtb16 interpret-uxtb16]
  [(#b1 #b10) uxtb   interpret-uxtb]
  [(#b1 #b11) uxth   interpret-uxth])
