#lang rosette

(require
  "../base.rkt"
  "../register.rkt"
  (prefix-in core: serval/lib/core))

(provide
  (all-defined-out))


; helper

(define-syntax (define-match-rule stx)
  (syntax-case stx ()
    [(_ (id args ...) body ...)
     #'(define-match-expander id
         (syntax-rules () [(_ args ...) body ...])
         (syntax-rules () [(_ args ...) body ...]))]))

(define (ereg? r)
  (if (and r (member r '(r8 r9 r10 r11 r12 r13 r14 r15)))
    #t #f))

(define (bitvector->modr/m x)
  ; Rosette can simplify one level of extract of concat.
  (define-values (mod reg)
    (match x
      [(expression (== concat) mod+reg r/m)
       #:when (&& ((bitvector 5) mod+reg) ((bitvector 3) r/m))
       (values (extract 4 3 mod+reg) (extract 2 0 mod+reg))]
      [_
       (values (extract 7 6 x) (extract 5 3 x))]))
  (define r/m (extract 2 0 x))
  (list mod reg r/m))

(define (bitvector->opcode+reg x)
  (list (extract 7 3 x) (extract 2 0 x)))

(define (bitvector->rex val)
  (define-values (magic w r x b)
    (match val
      [(expression (== concat) magic+w+r+x b)
       #:when (&& ((bitvector 7) magic+w+r+x) ((bitvector 1) b))
       (match magic+w+r+x
         [(expression (== concat) (expression (== concat) magic+w r) x)
          #:when (&& ((bitvector 5) magic+w) ((bitvector 1) r) ((bitvector 1) x))
          (values (extract 4 1 magic+w) (bit 0 magic+w) r x b)]
         [_
          (values (extract 6 3 magic+w+r+x) (bit 2 magic+w+r+x) (bit 1 magic+w+r+x) (bit 0 magic+w+r+x) b)])]
      [_
       (values (extract 7 4 val) (bit 3 val) (bit 2 val) (bit 1 val) (bit 0 val))]))
  (list magic w r x b))

(define (decode-imm . lst)
  (core:list->bitvector/le lst))

(define (encode-imm x)
  (core:bitvector->list/le x))

(define (encode-gpr-rex x)
  (cond
    [(false? x) x]
    [(bv? x) x]
    [else (car (register-encode x))]))

(define (encode-gpr-modr/m x)
  (cond
    [(bv? x) x]
    [else (cdr (register-encode x))]))


; byte

(define-match-expander byte
  (syntax-rules () [(_ x) (== (bv x 8))])
  (syntax-rules () [(_ x) (bv x 8)]))


; ModR/M: mod (2 bits) + reg (3 bits) + r/m (3 bits)

(define-match-expander modr/m
  (syntax-rules ()
    [(_ mod reg r/m)
     (app bitvector->modr/m (list mod reg r/m))])
  (syntax-rules ()
    [(_ mod reg r/m)
     (concat mod (encode-gpr-modr/m reg) (encode-gpr-modr/m r/m))]))

; /r: ModR/M with mod = 11
(define-match-expander /r
  (syntax-rules ()
    [(_ reg r/m)
     (modr/m (== (bv #b11 2)) reg r/m)])
  (syntax-rules ()
    [(_ reg r/m) (modr/m (bv #b11 2) reg r/m)]))

; /digit: the reg field contains the digit
(define-match-expander /digit
  (syntax-rules ()
    [(_ n r/m) (/r (== (bv n 3)) r/m)])
  (syntax-rules ()
    [(_ n r/m) (/r (bv n 3) r/m)]))

(define-match-rule (/0 r/m) (/digit 0 r/m))
(define-match-rule (/1 r/m) (/digit 1 r/m))
(define-match-rule (/2 r/m) (/digit 2 r/m))
(define-match-rule (/3 r/m) (/digit 3 r/m))
(define-match-rule (/4 r/m) (/digit 4 r/m))
(define-match-rule (/5 r/m) (/digit 5 r/m))
(define-match-rule (/6 r/m) (/digit 6 r/m))
(define-match-rule (/7 r/m) (/digit 7 r/m))


; opcode+reg

(define-match-expander +r
  (syntax-rules ()
    [(_ opcode reg)
     (app bitvector->opcode+reg (list (== (extract 7 3 (bv opcode 8))) reg))])
  (syntax-rules ()
    [(_ opcode gpr)
     (concat (extract 7 3 (bv opcode 8)) (cdr (register-encode gpr)))]))


; REX prefix

(define-match-expander rex
  (syntax-rules ()
    [(_ w r x b)
     (app bitvector->rex (list (== (bv #b0100 4)) w r x b))])
  (syntax-rules ()
    [(_ w r x b)
     (concat (bv #b0100 4) w (encode-gpr-rex r) x (encode-gpr-rex b))]))

(define-match-expander rex.w
  (syntax-rules ()
    [(_ r x b)
     (rex (== (bv #b1 1)) r x b)])
  (syntax-rules ()
    [(_ r x b)
     (rex (bv #b1 1) r x b)]))

(define-match-expander rex.w/r
  (syntax-rules ()
    [(_ r b)
     (rex.w r (== (bv #b0 1)) b)]
    [(_ b)
     (rex.w/r (== (bv #b0 1)) b)]
    [(_)
     (rex.w/r (== (bv #b0 1)))])
  (syntax-rules ()
    [(_ r b)
     (rex.w r (bv #b0 1) b)]
    [(_ b)
     (rex.w/r (bv #b0 1) b)]
    [(_)
     (rex.w/r (bv #b0 1))]))

(define-match-expander rex/r
  (syntax-rules ()
    [(_ r b)
     (rex (== (bv #b0 1)) r (== (bv #b0 1)) b)]
    [(_ b)
     (rex/r (== (bv #b0 1)) b)])
  (syntax-rules ()
    [(_ rr bb)
     (let ([r (encode-gpr-rex rr)]
           [b (encode-gpr-rex bb)])
       (if (and r b)
           (rex (bv #b0 1) r (bv #b0 1) b)
           null))]
    [(_ bb)
     (let ([b (encode-gpr-rex bb)])
       (if b (rex/r (bv #b0 1) b) null))]))
