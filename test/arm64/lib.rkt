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

(define choose-option
  (core:make-arg (bitvector 3)))

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
  (box (core:make-arg (bitvector 5))))

(define choose-sp
  (arm64:integer->gpr 31))


; configuration

(define CODE-ADDR #x100000)
(define DATA-ADDR #x4000000)
(define DATA-SIZE 16)

; unicorn helpers

(define (integer->xn i)
  (string->symbol (format "x~a" i)))

(define (make-memmgr)
  ; arbitrary doesn't work on UF, so fake a function for memory
  (define vec (list->vector (core:bitvector->list/le (arbitrary (core:make-arg (bitvector (* 8 DATA-SIZE)))))))
  (define (m i) (vector-ref vec (- (bitvector->natural i) DATA-ADDR)))
  (core:flat-memmgr m 64))

(define (cpu->uc cpu code data)
  (define uc (uc-open 'arm64 'arm))
  ; allocate memory for code/data
  (uc-mem-map uc CODE-ADDR 4096 'all)
  (uc-mem-map uc DATA-ADDR 4096 'all)
  ; write code/data
  (uc-mem-write uc CODE-ADDR code)
  (uc-mem-write uc DATA-ADDR data)
  ; set pc
  (uc-reg-write uc 'pc (bitvector->natural (arm64:cpu-pc-ref cpu)))
  ; set sp
  (uc-reg-write uc 'sp (bitvector->natural (arm64:cpu-sp-ref cpu)))
  ; set Xn
  (for ([i (range 31)])
    (uc-reg-write uc (integer->xn i) (bitvector->natural (arm64:cpu-gpr-ref cpu (arm64:integer->gpr i)))))
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
  (define data (core:list->bitvector/le (map (lambda (x) (bv x 8)) (bytes->list (uc-mem-read uc DATA-ADDR DATA-SIZE)))))
  (define mm (make-memmgr))
  (core:memmgr-store! mm (bv DATA-ADDR 64) (bv 0 64) data (bv DATA-SIZE 64))
  (arm64:cpu pc sp xn (arm64:bitvector->nzcv nzcv) mm))

(define (check-insn ctor generators)
  (define args (map arbitrary generators))
  (define insn (apply ctor args))

  (with-check-info [('insn insn)]
    (@check-insn insn)))

(define (@check-insn insn)
  (define code-addr (bv CODE-ADDR 64))
  (define data-addr (bv DATA-ADDR 64))
  (define data-size (bv DATA-SIZE 64))

  (define cpu (arbitrary (arm64:init-cpu (make-memmgr))))
  (define mm (arm64:cpu-memmgr cpu))
  (arm64:cpu-pc-set! cpu code-addr)

  ; set sp to data address for testing memory instructions
  (arm64:cpu-sp-set! cpu data-addr)

  ; initial data
  (define data-bytes (core:memmgr-load mm data-addr (bv 0 64) data-size))

  (define insn-bits (arm64:instruction-encode insn))
  ; each instruction is 32-bit
  (check-equal? (core:bv-size insn-bits) 32)
  ; check encode-decode consistency
  (check-equal? insn (arm64:decode insn-bits))

  (define code (list->bytes (map bitvector->natural (core:bitvector->list/le insn-bits))))
  (define data (list->bytes (map bitvector->natural (core:bitvector->list/le data-bytes))))
  (define uc (cpu->uc cpu code data))

  ; run the emulator
  (define uc-faulted? #f)
  (with-handlers ([exn:fail? (lambda (exn) (set! uc-faulted? #t))])
    (uc-emu-start uc CODE-ADDR (+ CODE-ADDR (bytes-length code)) 0 1))
  (define uc-cpu (uc->cpu uc))
  (define uc-mm (arm64:cpu-memmgr uc-cpu))

  ; if the emulator faulted (e.g., illegal instructions), the interpreter should also fault
  (define emu-pred (if uc-faulted? exn:fail? (lambda (x) #f)))

  ; run the interpreter
  (with-handlers ([emu-pred (lambda (exn) (clear-vc!))])
    (arm64:interpret-insn cpu insn))

  ; check if the final states match
  (check-equal? (arm64:cpu-pc-ref cpu) (arm64:cpu-pc-ref uc-cpu) "pc mismatch")
  (check-equal? (arm64:cpu-sp-ref cpu) (arm64:cpu-sp-ref uc-cpu) "sp mismatch")

  (for ([i (range 31)])
    (define r (arm64:integer->gpr i))
    (check-equal? (arm64:cpu-gpr-ref cpu r) (arm64:cpu-gpr-ref uc-cpu r) (format "X~a mismatch" i)))

  (check-equal? (arm64:cpu-nzcv-ref cpu) (arm64:cpu-nzcv-ref uc-cpu) "nzcv mismatch")

  (for ([i (range DATA-SIZE)])
    (check-equal? (core:memmgr-load (arm64:cpu-memmgr cpu) data-addr (bv i 64) (bv 1 64))
                  (core:memmgr-load uc-mm data-addr (bv i 64) (bv 1 64))
                  (format "memory mismatch at offset ~a" i))))


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
