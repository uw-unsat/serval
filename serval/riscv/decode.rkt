#lang rosette

(require
  (prefix-in core: "../lib/core.rkt")
  "../lib/bvarith.rkt"
  "base.rkt")

(provide
  add-decoder decode (struct-out nonzero))

(define decoders null)

; Recognizes a non-zero bitvector of a particular size.
(struct nonzero (size)
  #:transparent
  #:property prop:procedure
  (lambda (self v) (and ((bitvector (nonzero-size self)) v) (! (bvzero? v)))))

; To construct an instruction, a decoder checks that constant fields match
; and extracts values for symbolic fields:
; - ctor: an instruction struct;
; - spec: a list of bitvector types (symbolic fields) or values (constant fields).
(define (add-decoder ctor spec)
  ; split a 32-bit bitvector into chunks
  (define (split x lst)
    (define e (car lst))
    (when (box? e)
      (set! e (unbox e)))
    (set! lst (cdr lst))
    (define i
      (cond
        [(nonzero? e)   (nonzero-size e)]
        [(bitvector? e) (bitvector-size e)]
        [(bv? e)        (core:bv-size e)]
        [else           (error "unknown decoder")]))
    (define n (core:bv-size x))
    (cond
      [(&& (equal? i n) (empty? lst))
        ; The bitvector is the exact size as the next expected element
        (list x)]
      [(&& (< i n) (! (empty? lst)))
        ; There are more bytes in the bv than this part of the spec,
        ; recurse to continue decoding.
        (cons (trunc i x) (split (extract (sub1 n) i x) lst))]
      [else
        ; There are more bytes in the spec or there are remaining elements
        ; in the list. This means the bv is the wrong size according to spec
        ; so can never match.
        (list #f)]))

  (define (proc x)
    ; Match from lowest bits, which is a better fit for bytes
    ; constructed using (concat ...) and simpler for offsets.
    (define chunks (split x (reverse spec)))
    (cond
      ; Drop if split returns #f, when bv is wrong size
      [(false? (andmap (lambda (x) x) chunks)) #f]
      [else
        (set! chunks (reverse chunks))
        (define match? (andmap (lambda (act exp) (if (bv? exp) (bveq act exp) (exp act))) chunks spec))
        (define (make-if-bitvector act exp)
          (cond
            [(and (box? exp) (bitvector? (unbox exp))) (box act)]
            [(bitvector? exp) act]
            [(nonzero? exp) act]
            [else #f]))
        (if match?
            (apply ctor (filter-map make-if-bitvector chunks spec))
            #f)]))
  (set! decoders (cons proc decoders)))

(define (decode x)
  (define result (filter-map (lambda (proc) (proc x)) decoders))
  (when (null? result)
    (eprintf "no decoder for ~a\n" x)
    (eprintf "~a\n" (disassemble (core:bitvector->list/le x) #:arch "riscv64"))
    (exit 1))
  (when (! (= (length result) 1))
    (eprintf "ambiguity in decoding for ~a\n" x)
    (for ([e result])
      (eprintf "  ~a\n" e))
    (exit 1))
  (define insn (car result))
  (define exp-bytes (instruction-encode insn))
  (assert (equal? x exp-bytes)
          "encoded bytes must match original bytes")
  insn)

(define (disassemble #:arch [arch #f] lst)
  (define cmd (find-executable-path "llvm-mc"))
  (define args (list "--disassemble" "--show-inst"))
  (when arch
    (set! args (append (list "--arch" arch) args)))
  ; concretize input
  (set! lst (evaluate lst (complete-solution (sat) (symbolics lst))))
  (define in (open-input-string (string-join (map (lambda (x) (format "0x~x " (bitvector->natural x))) lst))))
  (define out (open-output-string))
  (parameterize ([current-input-port in]
                 [current-output-port out])
    (apply system* cmd args))
  (get-output-string out))