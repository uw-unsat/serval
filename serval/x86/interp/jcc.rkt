#lang rosette

(require
  "common.rkt")

(provide
  ja-rel8 jae-rel8
  jb-rel8 jbe-rel8
  je-rel8
  jg-rel8 jge-rel8
  jl-rel8 jle-rel8
  jne-rel8
  ja-rel32 jae-rel32
  jb-rel32 jbe-rel32
  je-rel32
  jg-rel32 jge-rel32
  jl-rel32 jle-rel32
  jne-rel32)

(define-syntax (define-jcc stx)
  (syntax-case stx ()
    [(_ id opcode proc)
     (with-syntax ([id-rel8 (format-id stx "~a-rel8" (syntax-e #'id))]
                   [id-rel32 (format-id stx "~a-rel32" (syntax-e #'id))])
       #'(begin
           (define-insn id-rel8 (rel8)
             #:decode [((byte opcode) i0)
                       (list (decode-imm i0))]
             #:encode (list (byte opcode) (encode-imm rel8))
             (lambda (cpu rel8)
               (when (proc cpu)
                 (cpu-pc-next! cpu (sign-extend rel8 (bitvector 64))))))
           (define-insn id-rel32 (rel32)
             #:decode [((byte #x0F) (byte (+ opcode #x10)) i0 i1 i2 i3)
                       (list (decode-imm i0 i1 i2 i3))]
             #:encode (list (byte #x0F) (byte (+ opcode #x10)) (encode-imm rel32))
             (lambda (cpu rel32)
               (when (proc cpu)
                 (cpu-pc-next! cpu (sign-extend rel32 (bitvector 64))))))))]))

; 77 cb
; 0F 87 cd
; Jump if above (CF=0 and ZF=0).
(define-jcc ja #x77
  (lambda (cpu)
    (&& (bveq (cpu-flag-ref cpu 'CF) (bv #b0 1))
        (bveq (cpu-flag-ref cpu 'ZF) (bv #b0 1)))))

; 73 cb
; 0F 83 cd
; Jump if above or equal (CF=0).
(define-jcc jae #x73
  (lambda (cpu)
    (bveq (cpu-flag-ref cpu 'CF) (bv #b0 1))))

; 72 cb
; 0F 82 cd
; Jump if below (CF=1).
(define-jcc jb #x72
  (lambda (cpu)
    (bveq (cpu-flag-ref cpu 'CF) (bv #b1 1))))

; 76 cb
; 0F 86 cd
; Jump if below or equal (CF=1 or ZF=1).
(define-jcc jbe #x76
  (lambda (cpu)
    (|| (bveq (cpu-flag-ref cpu 'CF) (bv #b1 1))
        (bveq (cpu-flag-ref cpu 'ZF) (bv #b1 1)))))

; 74 cb
; 0F 84 cd
; Jump if equal (ZF=1).
(define-jcc je #x74
  (lambda (cpu)
    (bveq (cpu-flag-ref cpu 'ZF) (bv #b1 1))))

; 7F cb
; 0F 8F cd
; Jump if greater (ZF=0 and SF=OF).
(define-jcc jg #x7F
  (lambda (cpu)
    (&& (bveq (cpu-flag-ref cpu 'ZF) (bv #b0 1))
        (bveq (cpu-flag-ref cpu 'SF) (cpu-flag-ref cpu 'OF)))))

; 7D cb
; 0F 8D cd
; Jump if greater or equal (SF=OF).
(define-jcc jge #x7D
  (lambda (cpu)
    (bveq (cpu-flag-ref cpu 'SF) (cpu-flag-ref cpu 'OF))))

; 7C cb
; 0F 8C cd
; Jump if less (SF≠OF).
(define-jcc jl #x7C
  (lambda (cpu)
    (! (bveq (cpu-flag-ref cpu 'SF) (cpu-flag-ref cpu 'OF)))))

; 7E cb
; 0F 8E cd
; Jump if less or equal (ZF=1 or SF≠OF).
(define-jcc jle #x7E
  (lambda (cpu)
    (|| (bveq (cpu-flag-ref cpu 'ZF) (bv #b1 1))
        (! (bveq (cpu-flag-ref cpu 'SF) (cpu-flag-ref cpu 'OF))))))

; 75 cb
; 0F 85 cd
; Jump if not equal (ZF=0).
(define-jcc jne #x75
  (lambda (cpu)
    (bveq (cpu-flag-ref cpu 'ZF) (bv #b0 1))))
