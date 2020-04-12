#lang rosette

(require
  (for-syntax (only-in racket/syntax format-id))
  rosette/lib/synthax
  serval/unicorn
  serval/lib/unittest
  (prefix-in core: serval/lib/core)
  (prefix-in arm64: serval/arm64))

(provide
  (all-defined-out)
  (all-from-out
    serval/lib/unittest))

; generators

(define choose-cond
  (core:make-arg (bitvector 4)))

(define choose-hw
  (core:make-arg (bitvector 2)))

(define choose-sf
  (core:make-arg (bitvector 1)))

(define choose-sh
  (core:make-arg (bitvector 1)))

(define choose-shift
  (core:make-arg (bitvector 2)))

(define choose-imm6
  (core:make-arg (bitvector 6)))

(define choose-imm12
  (core:make-arg (bitvector 12)))

(define choose-imm16
  (core:make-arg (bitvector 16)))

(define choose-imm19
  (core:make-arg (bitvector 19)))

(define choose-imm26
  (core:make-arg (bitvector 26)))

; X0 - X30, plus SP/ZR (31)
(define choose-reg
  (core:make-arg (bitvector 5)))


; unicorn helpers

(define (integer->xn i)
  (string->symbol (format "x~a" i)))

(define (cpu->uc cpu addr code)
  (define uc (uc-open 'arm64 'arm))
  ; allocate memory
  (uc-mem-map uc addr 4096 'all)
  ; write code
  (uc-mem-write uc addr code)
  ; set pc
  (uc-reg-write uc 'pc (bitvector->natural (arm64:cpu-pc-ref cpu)))
  ; set sp
  (uc-reg-write uc 'sp (bitvector->natural (arm64:cpu-sp-ref cpu)))
  ; set Xn
  (for ([i (range 31)])
    (uc-reg-write uc (integer->xn i) (bitvector->natural (arm64:cpu-gpr-ref cpu (bv i 5)))))
  ; set nzcv
  (uc-reg-write uc 'nzcv (bitvector->natural (arm64:nzcv->bitvector (arm64:cpu-nzcv-ref cpu))))
  uc)

(define (uc->cpu uc)
  (define pc (bv (uc-reg-read uc 'pc) 64))
  (define sp (bv (uc-reg-read uc 'sp) 64))
  (define xn
    (for/vector ([i (range 31)])
      (bv (uc-reg-read uc (integer->xn i)) 64)))
  (define nzcv (bv (uc-reg-read uc 'nzcv) 32))
  (arm64:cpu pc sp xn (arm64:bitvector->nzcv nzcv) #f))

(define (check-insn #:fixup [fixup void] ctor generators)
  (define args (map arbitrary generators))
  (define insn (apply ctor args))

  (define cpu (arbitrary (arm64:init-cpu #f)))
  (fixup insn cpu)

  (with-check-info [('insn insn)]
    (@check-insn cpu insn)))

(define (@check-insn cpu insn)
  (define addr #x100000)
  (arm64:cpu-pc-set! cpu (bv addr 64))

  (define insn-bits (arm64:instruction-encode insn))
  ; each instruction is 32-bit
  (check-equal? (core:bv-size insn-bits) 32)

  (define bstr (list->bytes (map bitvector->natural (core:bitvector->list/le insn-bits))))
  (define uc (cpu->uc cpu addr bstr))
  ; make sure the initial states match
  (check-equal? cpu (uc->cpu uc))

  ; run the emulator
  (define uc-faulted? #f)
  (with-handlers ([exn:fail? (lambda (exn) (set! uc-faulted? #t))])
    (uc-emu-start uc addr (+ addr (bytes-length bstr)) 0 1))
  (define uc-cpu (uc->cpu uc))

  ; if the emulator faulted (e.g., illegal instructions), the interpreter should also fault
  (define emu-pred (if uc-faulted? exn:fail? (lambda (x) #f)))

  ; run the interpreter
  (with-handlers ([emu-pred (lambda (exn) (clear-asserts!))])
    (arm64:interpret-insn cpu insn))

  (check-equal? (arm64:cpu-pc-ref cpu) (arm64:cpu-pc-ref uc-cpu) "pc mismatch")

  ; check if the final states match
  (check-equal? cpu uc-cpu))


; tests

(define-syntax (arm64-case stx)
  (syntax-case stx ()
    [(_ [generators ...] op)
     (with-syntax ([ctor (format-id stx "arm64:~a" (syntax-e #'op))])
       (syntax/loc stx
         (test-case+
           (symbol->string 'op)
           (quickcheck (check-insn ctor (list generators ...))))))]))

(define-syntax (arm64-case* stx)
  (syntax-case stx ()
    [(_ [generators ...] op ...)
     (syntax/loc stx
       (begin (arm64-case [generators ...] op) ...))]))
