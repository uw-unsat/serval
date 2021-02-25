#lang rosette

(require
  (for-syntax (only-in racket/syntax format-id))
  (only-in rosette/lib/synthax choose)
  (only-in rosette/lib/angelic choose*)
  serval/unicorn
  serval/lib/unittest
  (prefix-in core: serval/lib/core)
  (prefix-in x86: serval/x86))

(provide
  (all-defined-out)
  (all-from-out serval/lib/unittest))


; generators

; Use choose instead of choose* to make sure that registers in the same
; instruction choose consistently.
(define (choose-legacy x y)
  (choose x y))

(define (make-rex type)
  (type (core:make-arg (bitvector 1)) (core:make-arg (bitvector 3))))

(define (make-no-rex type)
  (type (core:make-arg (bitvector 3))))

(define (choose-imm8)
  (core:make-arg (bitvector 8)))

(define (choose-imm32)
  (core:make-arg (bitvector 32)))

(define (choose-imm64)
  (core:make-arg (bitvector 64)))

(define (choose-r8)
  (choose-legacy
    (choose-r8-rex)
    (make-no-rex x86:gpr8-no-rex)))

(define (choose-r8-rex)
  (make-rex x86:gpr8))

(define (choose-r16)
  (choose-legacy
    (choose-r16-rex)
    (make-no-rex x86:gpr16-no-rex)))

(define (choose-r16-rex)
  (make-rex x86:gpr16))

(define (choose-r32)
  (choose-legacy
    (make-rex x86:gpr32)
    (make-no-rex x86:gpr32-no-rex)))

(define (choose-r64)
  (make-rex x86:gpr64))

(define choose-r/m8
  choose-r8)

(define choose-r/m8-rex
  choose-r8-rex)

(define choose-r/m16
  choose-r16)

(define choose-r/m16-rex
  choose-r16-rex)

(define choose-r/m32
  choose-r32)

(define choose-r/m64
  choose-r64)

(define choose-rel8
  choose-imm8)

(define choose-rel32
  choose-imm32)

(define (choose-r8-imm8)
  (list (choose-r8) (choose-imm8)))

(define (choose-r8*-imm8)
  (list (choose-r8-rex) (choose-imm8)))

(define (choose-r32-imm32)
  (list (choose-r32) (choose-imm32)))

(define (choose-r32-r/m8)
  (list (choose-r32) (choose-r/m8)))

(define (choose-r32-r/m16)
  (list (choose-r32) (choose-r/m16)))

(define (choose-r32-r/m32)
  (list (choose-r32) (choose-r/m32)))

(define (choose-r64-imm64)
  (list (choose-r64) (choose-imm64)))

(define (choose-r64-r/m8)
  (list (choose-r64) (choose-r/m8-rex)))

(define (choose-r64-r/m16)
  (list (choose-r64) (choose-r/m16-rex)))

(define (choose-r64-r/m64)
  (list (choose-r64) (choose-r/m64)))

(define (choose-r/m16-imm8)
  (list (choose-r/m16) (choose-imm8)))

(define (choose-r/m32-imm8)
  (list (choose-r/m32) (choose-imm8)))

(define (choose-r/m32-imm32)
  (list (choose-r/m32) (choose-imm32)))

(define (choose-r/m32-r32)
  (list (choose-r/m32) (choose-r32)))

(define (choose-r/m32-r32-imm8)
  (list (choose-r/m32) (choose-r32) (choose-imm8)))

(define (choose-r/m64-imm8)
  (list (choose-r/m64) (choose-imm8)))

(define (choose-r/m64-imm32)
  (list (choose-r/m64) (choose-imm32)))

(define (choose-r/m64-r64)
  (list (choose-r/m64) (choose-r64)))

(define (choose-r/m64-r64-imm8)
  (list (choose-r/m64) (choose-r64) (choose-imm8)))

(define (choose-r/m64-no-rex)
  (list (make-no-rex x86:gpr64-no-rex)))

; unicorn helpers

(define (cpu->uc cpu addr code)
  (define uc (uc-open 'x86 'x86-64))
  ; allocate memory
  (uc-mem-map uc addr 4096 'all)
  ; write code
  (uc-mem-write uc addr code)
  ; set pc
  (uc-reg-write uc 'rip (bitvector->natural (x86:cpu-pc-ref cpu)))
  ; set gprs
  (for ([r x86:gprs64])
    (uc-reg-write uc r (bitvector->natural (x86:cpu-gpr-ref cpu (x86:symbol->gpr64 r)))))
  ; set rflags
  (uc-reg-write uc 'eflags (bitvector->natural (apply concat (reverse (vector->list (x86:cpu-flags cpu))))))
  uc)

(define (uc->cpu uc)
  (define pc (bv (uc-reg-read uc 'rip) 64))
  (define gprs
    (for/vector ([r x86:gprs64])
      (bv (uc-reg-read uc r) 64)))
  (define flags (bv (uc-reg-read uc 'eflags) 32))
  (x86:cpu pc gprs (list->vector (map (lambda (i) (bit i flags)) (range 32))) #f))

(define (check-insn #:fixup [fixup void] ctor args)
  (define insn (arbitrary (apply ctor args)))

  (define cpu (arbitrary (x86:init-cpu #f)))
  (fixup insn cpu)

  (with-check-info [('insn insn)]
    (@check-insn cpu insn)))

(define (@check-insn cpu insn)
  ; check encode-decode consistency
  (define insn-bytes (x86:instruction-encode insn))
  (check-equal? (list insn) (x86:decode insn-bytes))

  (define addr #x100000)
  (x86:cpu-pc-set! cpu (bv addr 64))

  (define bstr (list->bytes (map bitvector->natural insn-bytes)))
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
    (x86:interpret-insn cpu insn))

  ; check if the pc match
  (check-equal? (x86:cpu-pc-ref cpu) (x86:cpu-pc-ref uc-cpu) "pc mismatch")

  ; check if the gprs match
  (for ([r x86:gprs64])
    (check-equal? (x86:cpu-gpr-ref cpu (x86:symbol->gpr64 r))
                  (x86:cpu-gpr-ref uc-cpu (x86:symbol->gpr64 r))
                  (format "~a mismatch" r)))

  ; check if the flags match
  (for ([r x86:flags])
    (define flag (x86:cpu-flag-ref cpu r))
    (define uc-flag (x86:cpu-flag-ref uc-cpu r))
    ; Skip if flag is undefined (#f)
    (when flag
      (check-equal? flag uc-flag (format "~a mismatch" r)))))


; tests

(define-syntax (x86-case stx)
  (syntax-case stx ()
   [(_ op)
    (with-syntax ([ctor (format-id stx "x86:~a" (syntax-e #'op))]
                  [generator (format-id stx "choose-~a"
                               (string-join
                                 (filter-not (lambda (s) (member s (list "1" "eax" "rax" "cl")))
                                   (cdr (string-split (symbol->string (syntax-e #'op)) "-"))) "-"))])
      (syntax/loc stx
        (test-case+
          (symbol->string 'op)
          (quickcheck (check-insn ctor (flatten (generator)))))))]))

(define-syntax (x86-suite stx)
  (syntax-case stx ()
    [(_ name op ...)
     (syntax/loc stx
       (test-suite+ name (x86-case op) ...))]))
