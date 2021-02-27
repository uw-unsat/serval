#lang rosette

(require
  (for-syntax (only-in racket/syntax format-id))
  serval/unicorn
  serval/lib/unittest
  (prefix-in core: serval/lib/core)
  (prefix-in arm32: "../../serval/arm32.rkt"))

(provide
  (all-defined-out)
  (all-from-out
    serval/lib/core
    serval/lib/unittest
    "../../serval/arm32.rkt"))


; generators

(define choose-S
  (core:make-arg (bitvector 1)))

(define choose-cond
  ; focus on testing AL
  (bv #b1110 4))

(define choose-imm4
  (core:make-arg (bitvector 4)))

(define choose-imm5
  (core:make-arg (bitvector 5)))

(define choose-imm12
  (core:make-arg (bitvector 12)))

(define choose-imm24
  (core:make-arg (bitvector 24)))

(define choose-reg
  (box (core:make-arg (bitvector 4))))

(define choose-reg/no-r15
  (let ([v (core:make-arg (bitvector 4))])
    (box (if (bveq v (bv 15 4)) (bv 0 4) v))))

(define choose-rotate
  (core:make-arg (bitvector 2)))

(define choose-stype
  (core:make-arg (bitvector 2)))


; unicorn helpers

(define (integer->rn i)
  (string->symbol (format "r~a" i)))

(define (cpu->uc cpu addr code)
  (define uc (uc-open 'arm 'arm))
  ; allocate memory
  (uc-mem-map uc addr 4096 'all)
  ; write code
  (uc-mem-write uc addr code)
  ; set cpsr
  (uc-reg-write uc 'cpsr (bitvector->natural (arm32:pstate->bitvector (arm32:cpu-cpsr cpu))))
  ; set pc
  (uc-reg-write uc 'pc (bitvector->natural (arm32:cpu-pc cpu)))
  ; set registers
  (for ([i (range 15)])
    (uc-reg-write uc (integer->rn i) (bitvector->natural (arm32:cpu-gpr-ref cpu (arm32:integer->gpr i)))))
  uc)

(define (uc->cpu uc)
  (define pc (bv (uc-reg-read uc 'pc) 32))
  (define rs
    (for/vector ([i (range 15)])
      (bv (uc-reg-read uc (integer->rn i)) 32)))
  (define cpsr (bv (uc-reg-read uc 'cpsr) 32))
  (arm32:cpu pc rs (arm32:bitvector->pstate cpsr) #f))

(define (check-insn ctor generators)
  (define args (map arbitrary generators))
  (define insn (apply ctor args))

  (define cpu (arbitrary (arm32:init-cpu #f)))

  (with-check-info [('insn insn)]
    (@check-insn cpu insn)))

(define (@check-insn cpu insn)
  (define addr #x100000)
  (arm32:cpu-pc-set! cpu (bv addr 32))

  (define insn-bits (arm32:instruction-encode insn))
  ; set up condition code
  (when (equal? (core:bv-size insn-bits) 28)
    (set! insn (arm32:conditional-instruction (arbitrary choose-cond) insn))
    (set! insn-bits (arm32:instruction-encode insn)))

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
  (with-handlers ([emu-pred (lambda (exn) (clear-vc!))])
    (arm32:interpret-insn cpu insn))

  ; check if R[] matches
  (for ([i (range 15)])
    (define r (arm32:integer->gpr i))
    (check-equal? (arm32:cpu-gpr-ref cpu r) (arm32:cpu-gpr-ref uc-cpu r) (format "r~a mismatch" i)))

  ; check if cpsr matches
  (check-equal? (arm32:cpu-cpsr cpu) (arm32:cpu-cpsr uc-cpu) "cpsr mismatch")

  ; check if pc matches
  (define pc (arm32:cpu-pc cpu))
  (define uc-pc (arm32:cpu-pc uc-cpu))
  ; Unicorn sometimes doesn't correctly bump the PC; skip if that happens.
  (unless (&& (bveq pc (bv (+ addr 4) 32)) (bveq uc-pc (bv addr 32)))
    (check-equal? (arm32:cpu-pc cpu) (arm32:cpu-pc uc-cpu) "pc mismatch")))


; tests

(define-syntax (arm32-case stx)
  (syntax-case stx ()
    [(_ [generators ...] op)
     (with-syntax ([ctor (if (symbol? (syntax-e #'op))
                             (format-id stx "arm32:~a" (syntax-e #'op))
                             #'op)])
       (syntax/loc stx
         (test-case+
           (format "~a" 'op)
           (quickcheck (check-insn ctor (list generators ...))))))]))

(define-syntax (arm32-case* stx)
  (syntax-case stx ()
    [(_ [generators ...] op ...)
     (syntax/loc stx
       (begin (arm32-case [generators ...] op) ...))]))
