#lang racket

(require racket/struct
         syntax/strip-context)

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (literal-read in)
  (syntax->datum
    (literal-read-syntax #f in)))

(define (literal-read-syntax src in)
  (strip-context
    #`(module anything racket/base
        (provide globals)
        (require serval/lib/core)
        (define globals
          (make-hash
            (list #,@(read-globals (port->dies in))))))))

(define (read-globals dies)
  ; build offset->die table
  (define h (make-hash))
  (for ([cu dies])
    (update-offset-die! h cu))
  ; scan for top-level variables
  (for*/list ([cu dies]
              [e (die-children cu)]
              #:when (and (equal? (die-tag e) 'DW_TAG_variable)
                          (attribute-has-key? e 'DW_AT_name)))
    (define name (attribute-name e))
    (define type (read-type h (attribute-type e)))
    (with-syntax ([t type]
                  [n name])
      #'(cons 'n (lambda () t)))))

; build offset->die lookup table
(define (update-offset-die! h e)
  (hash-set! h (die-offset e) e)
  (for ([c (die-children e)])
    (update-offset-die! h c)))

(define (read-type h e)
  ; look up e if given offset
  (when (not (die? e))
    (set! e (hash-ref h e)))
  (case (die-tag e)
    [(DW_TAG_array_type)
     ; scan through all array dimensions
     (define dims
       (for/list ([subrange (die-children e)])
         (unless (equal? (die-tag subrange) 'DW_TAG_subrange_type)
           (error 'dwarf "not subrange: ~a" subrange))
         (if (attribute-has-key? subrange 'DW_AT_upper_bound)
             (add1 (attribute-upper-bound subrange))
             0)))
     (define elements (read-type h (attribute-type e)))
     (foldr (lambda (len elem) `(marray ,len ,elem)) elements dims)]
    [(DW_TAG_structure_type)
     (define size (attribute-byte-size e))
     (define fields
       (append (list 'list)
        (for/list [(c (die-children e))]
          (read-type h c))))
     `(mstruct ,size ,fields)]
    [(DW_TAG_member)
     (define name (attribute-name e))
     (define offset (attribute-data-member-location e))
     (define element (read-type h (attribute-type e)))
     `(mfield ',name ,offset ,element)]
    [(DW_TAG_base_type DW_TAG_pointer_type)
     (define size (attribute-byte-size e))
     `(mcell ,size)]
    [(DW_TAG_typedef DW_TAG_const_type)
     (read-type h (attribute-type e))]
    ; FIXME: volatile cell
    [(DW_TAG_volatile_type)
     (read-type h (attribute-type e))]
    [else
     (error 'dwarf "unknown type: ~a" e)]))

(define (attribute-has-key? e key)
  (dict-has-key? (die-attributes e) key))

(define (attribute-ref e key)
  (dict-ref (die-attributes e) key #f))

(define (attribute-number-ref e key)
  (define s (attribute-ref e key))
  ; hex number
  (when (string-prefix? s "0x")
    (set! s (string-append "#" (substring s 1))))
  (define result (string->number s))
  (unless result
    (error 'dwarf "malformed number for ~a: ~a" key e))
  result)

(define (attribute-byte-size e)
  (attribute-number-ref e 'DW_AT_byte_size))

(define (attribute-data-member-location e)
  (attribute-number-ref e 'DW_AT_data_member_location))

(define (attribute-upper-bound e)
  (attribute-number-ref e 'DW_AT_upper_bound))

(define (attribute-name e)
  (define s (attribute-ref e 'DW_AT_name))
  (string->symbol
    (match s
      ; indirect name
      [(pregexp #px"\\([^\\)]+\\):\\s*(.*)" (list _ name)) name]
      [_ s])))

(define (attribute-type e)
  (define type (attribute-ref e 'DW_AT_type))
  (match type
    [(pregexp #px"^<0x([0-9a-f]+)>$" (list _ offset))
     (string->symbol offset)]
    [_
     (error 'dwarf "unknown variable type: ~a" type)]))


; parse debugging information entries (DIEs) from objdump

(struct die (tag level offset attributes parent children)
 #:transparent #:mutable
 #:methods gen:custom-write
 [(define write-proc
   (make-constructor-style-printer
     (lambda (e) 'die)
     (lambda (e) (list (die-tag e) (die-level e) (die-offset e) (die-attributes e)))))])

(define (port->dies in)
  ; create a fake root to collect compile units
  (define root (die #f #f #f #f #f null))
  (parse-dies (port->lines in) root -1)
  (die-children root))

(define (parse-dies lines current-die current-level)
  (define-values (e rest) (parse-die lines))
  (when e
    (define level (die-level e))
    (cond
      ; e is one level down
      [(> level current-level)
       (when (not (equal? level (add1 current-level)))
         (error 'dwarf "incorrect level: ~a -> ~a" current-level level))]
      ; e is at the same level or higher
      [else
       ; locate the parent die
       (for ([l (in-range current-level (sub1 level) -1)])
         (set! current-die (die-parent current-die)))])
    ; link e to the die tree
    (set-die-parent! e current-die)
    (set-die-children! current-die (append (die-children current-die) (list e)))
    ; recursively parse
    (parse-dies rest e level)))

(define (parse-die lines)
  (if (empty? lines)
      (values #f lines)
      (match lines
        [(cons (pregexp #px"^\\s*<([0-9]+)><([0-9a-f]+)>: Abbrev Number: (\\d+) \\((\\w+)\\)$" (list _ level offset abbrev tag)) rest)
         (define attrs (parse-attributes rest))
         (define e (die (string->symbol tag) (string->number level) (string->symbol offset) attrs #f null))
         (values e (drop rest (length attrs)))]
        [_
         ; (displayln (format "skip: ~a" (car lines)))
         (parse-die (cdr lines))])))

(define (parse-attributes lines)
  (match lines
   [(cons (pregexp #px"^\\s*<[0-9a-f]+>\\s*(\\w+)\\s*:\\s*(.*)$" (list _  key value)) rest)
    (cons (cons (string->symbol key) value) (parse-attributes rest))]
   [_ null]))
