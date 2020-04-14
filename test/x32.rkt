#lang rosette

(require (only-in racket/syntax format-id)
         rosette/lib/synthax
         serval/unicorn
         serval/lib/unittest
         (prefix-in core: serval/lib/core)
         (prefix-in x32: serval/x32)
         (prefix-in x32: serval/x32/base)
         serval/x32/interp)

(define choose-imm8
  (core:make-arg (bitvector 8)))

(define choose-imm32
  (core:make-arg (bitvector 32)))

(define choose-r8
  (choose 'al 'cl 'dl 'bl 'ah 'ch 'dh 'bh))

(define choose-r32
  (choose 'eax 'ecx 'edx 'ebx 'esp 'ebp 'esi 'edi))

(define choose-r32-minus-edx
  (choose 'eax 'ecx 'ebx 'esp 'ebp 'esi 'edi))

(define (cpu->uc cpu addr code)
  (define uc (uc-open 'x86 'x86-32))
  ; allocate memory
  (uc-mem-map uc addr 4096 'all)
  ; write code
  (uc-mem-write uc addr code)
  ; set gprs
  (for ([gpr '(eax ecx edx ebx esp ebp esi edi)])
    (uc-reg-write uc gpr (bitvector->natural (x32:gpr-ref cpu gpr))))
  ; set eflags
  (uc-reg-write uc 'eflags (bitvector->natural (x32:flags->bitvector (x32:cpu-flags cpu))))
  uc)

(define (uc->cpu uc)
  (define pc (bv (uc-reg-read uc 'eip) 32))
  (define regs
    (for/list ([gpr '(eax ecx edx ebx esp ebp esi edi)])
      (bv (uc-reg-read uc gpr) 32)))
  (define eflags (bv (uc-reg-read uc 'eflags) 32))
  (x32:cpu pc (apply x32:gprs regs) (x32:bitvector->flags eflags) (core:typed-bv-memmgr null)))

(define (check-insn #:fixup [fixup void] ctor . generators)
  (define args (map arbitrary generators))
  (define insn (apply ctor args))
  (check-equal? insn (x32:decode (x32:instruction-encode insn)))

  ; initialize
  (define cpu (arbitrary (x32:init-cpu)))
  (fixup insn cpu)

  (define bstr (list->bytes (x32:instruction->integer-bytes insn)))
  (define addr #x100000)
  (define uc (cpu->uc cpu addr bstr))
  ; make sure the initial states match
  (check-equal? cpu (uc->cpu uc))

  ; run the emulator
  (with-handlers ([exn:fail? (lambda (exn) (void))])
    (uc-emu-start uc addr (+ addr (bytes-length bstr)) 0 1))
  (define uc-cpu (uc->cpu uc))

  ; run the interpreter
  (x32:set-cpu-pc! cpu (bv addr 32))
  (x32:interpret-instr cpu insn)

  ; no symbolic register values
  (for ([r '(eax ecx edx ebx esp ebp esi edi)])
    (check-false (term? (x32:gpr-ref cpu r))))

  ; if the spec sets a symbolic flag, replace it with a concrete one from the emulator
  ; this is faster than invoking the solver
  (for ([flag '(CF PF AF ZF SF OF)])
    (when (constant? (x32:flag-ref cpu flag))
      (x32:flag-set! cpu flag (x32:flag-ref uc-cpu flag))))

  ; check if the final states match
  (check-equal? cpu uc-cpu))

; fixup for overflow cases

(define (fixup-adc cpu dst v2)
  (define v1 (x32:gpr-ref cpu dst))
  (define choose-val
    (choose v1
            (bvsub (bv #xffffffff 32) v2)
            (bvsub (bv #x7fffffff 32) v2)))
    (x32:gpr-set! cpu dst (arbitrary choose-val)))

(define (fixup-adc-r32-imm8 insn cpu)
  (match-let ([(adc-r32-imm8 dst imm8) insn])
    (fixup-adc cpu dst (sign-extend imm8 (bitvector 32)))))

(define (fixup-adc-r32-r32 insn cpu)
  (match-let ([(adc-r32-r32 dst src) insn])
    (when (! (equal? dst src))
      (fixup-adc cpu dst (x32:gpr-ref cpu src)))))

(define (fixup-div-r32 insn cpu)
  (x32:gpr-set! cpu 'edx (bv 0 32))
  (match-let ([(div-r32 src) insn])
    (when (bvzero? (x32:gpr-ref cpu src))
      (x32:gpr-set! cpu src (bv 1 32)))))

(define (fixup-sbb cpu dst v2)
  (define v1 (x32:gpr-ref cpu dst))
  (define choose-val
    (choose v1
            v2
            (bvadd (bv #x80000000 32) v2)))
    (x32:gpr-set! cpu dst (arbitrary choose-val)))

(define (fixup-sbb-r32-imm8 insn cpu)
  (match-let ([(sbb-r32-imm8 dst imm8) insn])
    (fixup-sbb cpu dst (sign-extend imm8 (bitvector 32)))))

(define (fixup-sbb-r32-r32 insn cpu)
  (match-let ([(sbb-r32-r32 dst src) insn])
  (when (! (equal? dst src))
    (fixup-sbb cpu dst (x32:gpr-ref cpu src)))))

(define-syntax-rule (x32-case opcode args ...)
  (test-case+ (symbol->string 'opcode)
              (quickcheck (check-insn opcode args ...))))

(define x32-tests
  (test-suite+ "Tests for x32 instructions"
     (x32-case adc-r32-imm8 choose-r32 choose-imm8 #:fixup fixup-adc-r32-imm8)
     (x32-case adc-r32-r32 choose-r32 choose-r32 #:fixup fixup-adc-r32-r32)
     (x32-case add-r32-imm8 choose-r32 choose-imm8)
     (x32-case add-r32-r32 choose-r32 choose-r32)
     (x32-case and-r32-imm8 choose-r32 choose-imm8)
     (x32-case and-r/m32-r32 choose-r32 choose-r32)
     (x32-case and-r32-r/m32 choose-r32 choose-r32)
     (x32-case cmp-r32-imm8 choose-r32 choose-imm8)
     (x32-case cmp-r/m32-r32 choose-r32 choose-r32)
     (x32-case div-r32 choose-r32-minus-edx #:fixup fixup-div-r32)
     (x32-case ja-rel8 choose-imm8)
     (x32-case jae-rel8 choose-imm8)
     (x32-case jb-rel8 choose-imm8)
     (x32-case jbe-rel8 choose-imm8)
     (x32-case je-rel8 choose-imm8)
     (x32-case jg-rel8 choose-imm8)
     (x32-case jge-rel8 choose-imm8)
     (x32-case jl-rel8 choose-imm8)
     (x32-case jle-rel8 choose-imm8)
     (x32-case jne-rel8 choose-imm8)
     (x32-case jmp-rel8 choose-imm8)
     (x32-case ja-rel32 choose-imm32)
     (x32-case jae-rel32 choose-imm32)
     (x32-case jb-rel32 choose-imm32)
     (x32-case jbe-rel32 choose-imm32)
     (x32-case je-rel32 choose-imm32)
     (x32-case jg-rel32 choose-imm32)
     (x32-case jge-rel32 choose-imm32)
     (x32-case jl-rel32 choose-imm32)
     (x32-case jle-rel32 choose-imm32)
     (x32-case jne-rel32 choose-imm32)
     (x32-case jmp-rel32 choose-imm32)
     (x32-case mov-r8-imm8 choose-r8 choose-imm8)
     (x32-case mov-r32-imm32 choose-r32 choose-imm32)
     (x32-case mov-r/m32-r32 choose-r32 choose-r32)
     (x32-case mov-r32-r/m32 choose-r32 choose-r32)
     (x32-case movzx-r32-r8 choose-r32 choose-r8)
     (x32-case mul-r32 choose-r32)
     (x32-case neg-r32 choose-r32)
     (x32-case or-r32-imm8 choose-r32 choose-imm8)
     (x32-case or-r32-r32 choose-r32 choose-r32)
     (x32-case sar-r32-cl choose-r32)
     (x32-case sar-r32-imm8 choose-r32 choose-imm8)
     (x32-case sbb-r32-imm8 choose-r32 choose-imm8 #:fixup fixup-sbb-r32-imm8)
     (x32-case sbb-r32-r32 choose-r32 choose-r32 #:fixup fixup-sbb-r32-r32)
     (x32-case shl-r32-cl choose-r32)
     (x32-case shl-r32-imm8 choose-r32 choose-imm8)
     (x32-case shr-r32-cl choose-r32)
     (x32-case shr-r32-imm8 choose-r32 choose-imm8)
     (x32-case shld-r32-r32-cl choose-r32 choose-r32)
     (x32-case shld-r32-r32-imm8 choose-r32 choose-r32 choose-imm8)
     (x32-case shrd-r32-r32-cl choose-r32 choose-r32)
     (x32-case shrd-r32-r32-imm8 choose-r32 choose-r32 choose-imm8)
     (x32-case sub-r32-imm8 choose-r32 choose-imm8)
     (x32-case sub-r32-r32 choose-r32 choose-r32)
     (x32-case xor-r32-imm8 choose-r32 choose-imm8)
     (x32-case xor-r/m32-r32 choose-r32 choose-r32)
     (x32-case xor-r32-r/m32 choose-r32 choose-r32)
  ))

(module+ test
  (time (run-tests x32-tests)))
