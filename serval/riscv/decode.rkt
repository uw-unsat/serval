#lang rosette

(require
  (prefix-in core: "../lib/core.rkt")
  "../lib/bvarith.rkt"
  "base.rkt")

(provide
  add-decoder decode (struct-out exclude) nonzero disassemble)

(define decoders null)

; (exclude elems size) is guard that recognizes bitvectors of a size
; excluding some list of (concrete) values.
(struct exclude (elems size)
  #:transparent
  #:property prop:procedure
  (lambda (self v)
    (let ([size (exclude-size self)]
          [elems (exclude-elems self)])
      (and ((bitvector size) v)
           (andmap (lambda (x) (not (equal? (bv x size) v)))
                   elems)))))

; Recognizes a non-zero bitvector of a particular size.
(define (nonzero size) (exclude (list 0) size))

; To construct an instruction, a decoder checks that constant fields match
; and extracts values for symbolic fields:
; - ctor: an instruction struct;
; - spec: a list of bitvector types (symbolic fields) or values (constant fields).
(define (add-decoder mode ctor spec)
  ; split a 32-bit bitvector into chunks
  (define (split x lst)
    (define e (car lst))
    (when (box? e)
      (set! e (unbox e)))
    (set! lst (cdr lst))
    (define i
      (cond
        [(exclude? e)   (exclude-size e)]
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
      ; Not the right XLEN for this instruction.
      [(not (member (XLEN) mode)) #f]
      ; Drop if split returns #f, when bv is wrong size
      [(false? (andmap (lambda (x) x) chunks)) #f]
      [else
        (set! chunks (reverse chunks))
        (define match? (andmap (lambda (act exp) (if (bv? exp) (bveq act exp) (exp act))) chunks spec))
        (define (make-if-bitvector act exp)
          (cond
            [(and (box? exp) (bitvector? (unbox exp))) (box act)]
            [(bitvector? exp) act]
            [(exclude? exp) act]
            [else #f]))
        (if match?
            (apply ctor (filter-map make-if-bitvector chunks spec))
            #f)]))
  (set! decoders (cons proc decoders)))

(define (decode x)
  (define result (filter-map (lambda (proc) (proc x)) decoders))
  (when (null? result)
    (eprintf "no decoder for ~a\n" x)
    (eprintf "~a\n" (disassemble (core:bitvector->list/le x) #:arch "riscv:rv64"))
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

(define (disassemble #:arch [arch "riscv:rv64"] lst)

  ; concretize input
  (set! lst (evaluate lst (complete-solution (sat) (symbolics lst))))
  ; convert to bin
  (define bin (bytes-join (map (lambda (x) (bytes (bitvector->natural x))) lst) #""))
  (define tmpname (make-temporary-file))

  ; write to output
  (call-with-output-file* tmpname #:exists 'truncate
    (lambda (out) (display bin out)))
  (define cmd (find-executable-path "riscv64-unknown-elf-objdump"))
  (define args (list "-M" "no-aliases" "-b" "binary" "-m" arch "-D" tmpname))
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (apply system* cmd args))
  (get-output-string out))
