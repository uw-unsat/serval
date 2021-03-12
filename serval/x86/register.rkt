#lang rosette

(require
  rosette/lib/match
  (prefix-in core: "../lib/core.rkt")
  "base.rkt")

(provide
  gpr64 gpr64-no-rex
  gpr32 gpr32-no-rex
  gpr16 gpr16-no-rex
  gpr8 gpr8-no-rex
  gprs64 symbol->gpr64
  symbol->gpr32
  rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15
  eax ecx edx ebx esp ebp esi edi
  cl
  register-indirect)


(define (integer->gpr i ctor)
  (define rex (bv (quotient i 8) 1))
  (define reg (bv i 3))
  (case (procedure-arity ctor)
    [(1) (ctor reg)]
    [(2) (ctor rex reg)]))

(define-syntax (define-gpr stx)
  (syntax-case stx ()
    [(_ id (args ...) #:size size #:names names #:getter getter #:setter setter)
     #'(struct id (args ...)
         #:transparent
         #:methods gen:register
         [(define (register-size gpr) size)
          (define (register-name gpr)
            (define lst
              (for/list ([i (range (length names))]
                         [r names])
                (cons (integer->gpr i id) r)))
            (cdr (assoc gpr lst)))
          (define (register-encode gpr)
            (match gpr
              [(id args ...)
               ; FIXME: just be compatible for now.
               ; Should find a better way.
               (case (length (list args ...))
                 [(1) (cons #f args ...)]
                 [(2) (cons args ...)])]))
          (define (register-ref cpu gpr)
            (define lst
              (for/list ([i (range (length names))])
                (cons (integer->gpr i id) (getter i))))
            (define proc (cdr (assoc gpr lst)))
            (proc (cpu-gprs cpu)))
          (define (register-set! cpu gpr v)
            (define lst
              (for/list ([i (range (length names))])
                (cons (integer->gpr i id) (setter i v))))
            (define proc (cdr (assoc gpr lst)))
            (proc (cpu-gprs cpu)))])]))


; gpr64

(define gprs64 '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

(define (gpr64-getter i)
  (lambda (vec)
    (vector-ref vec i)))

(define (gpr64-setter i v)
  (lambda (vec)
    (vector-set! vec i v)))

(define-gpr gpr64 (rex reg)
  #:size 64
  #:names gprs64
  #:getter gpr64-getter
  #:setter gpr64-setter)

(define-gpr gpr64-no-rex (reg)
  #:size 64
  #:names (take gprs64 8)
  #:getter gpr64-getter
  #:setter gpr64-setter)


; gpr32

(define gprs32 '(eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d))

(define (gpr32-getter i)
  (lambda (vec)
    (extract 31 0 (vector-ref vec i))))

(define (gpr32-setter i v)
  (lambda (vec)
    (vector-set! vec i (zero-extend v (bitvector 64)))))

(define-gpr gpr32 (rex reg)
  #:size 32
  #:names gprs32
  #:getter gpr32-getter
  #:setter gpr32-setter)

(define-gpr gpr32-no-rex (reg)
  #:size 32
  #:names (take gprs32 8)
  #:getter gpr32-getter
  #:setter gpr32-setter)


; gpr16

(define gprs16 '(ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w))

(define (gpr16-getter i)
  (lambda (vec)
    (extract 15 0 (vector-ref vec i))))

(define (gpr16-setter i v)
  (lambda (vec)
    (vector-set! vec i (concat (extract 63 16 (vector-ref vec i)) v))))

(define-gpr gpr16 (rex reg)
  #:size 16
  #:names gprs16
  #:getter gpr16-getter
  #:setter gpr16-setter)

(define-gpr gpr16-no-rex (reg)
  #:size 16
  #:names (take gprs16 8)
  #:getter gpr16-getter
  #:setter gpr16-setter)


; gpr8

(define gprs8 '(al cl dl bl spl bpl sil dil r8b r9b r10b r11b r12b r13b r14b r15b))

(define-gpr gpr8 (rex reg)
  #:size 8
  #:names gprs8
  #:getter
    (lambda (i)
      (lambda (vec)
        (extract 7 0 (vector-ref vec i))))
  #:setter
    (lambda (i v)
      (lambda (vec)
        (vector-set! vec i (concat (extract 63 8 (vector-ref vec i)) v)))))

(define gprs8-no-rex '(al cl dl bl ah ch dh bh))

(define-gpr gpr8-no-rex (reg)
  #:size 8
  #:names gprs8-no-rex
  #:getter
    (lambda (i)
      (lambda (vec)
        (cond
          [(< i 4)
           (extract 7 0 (vector-ref vec i))]
          [else
           (extract 15 8 (vector-ref vec (- i 4)))])))
  #:setter
    (lambda (i v)
      (lambda (vec)
        (cond
          [(< i 4)
           (vector-set! vec i (concat (extract 63 8 (vector-ref vec i)) v))]
          [else
           (let ([old (vector-ref vec (- i 4))])
             (vector-set! vec (- i 4) (concat (extract 63 16 old) v (extract 7 0 old))))]))))


(define (symbol->gpr64 sym)
  (integer->gpr (index-of gprs64 sym) gpr64))

(define (symbol->gpr32 sym)
  (integer->gpr (index-of gprs32 sym) gpr32))

(define (symbol->gpr8 sym)
  (integer->gpr (index-of gprs8 sym) gpr8))

; 64-bit GPRs
(define rax (symbol->gpr64 'rax))
(define rcx (symbol->gpr64 'rcx))
(define rdx (symbol->gpr64 'rdx))
(define rbx (symbol->gpr64 'rbx))
(define rsp (symbol->gpr64 'rsp))
(define rbp (symbol->gpr64 'rbp))
(define rsi (symbol->gpr64 'rsi))
(define rdi (symbol->gpr64 'rdi))
(define r8 (symbol->gpr64 'r8))
(define r9 (symbol->gpr64 'r9))
(define r10 (symbol->gpr64 'r10))
(define r11 (symbol->gpr64 'r11))
(define r12 (symbol->gpr64 'r12))
(define r13 (symbol->gpr64 'r13))
(define r14 (symbol->gpr64 'r14))
(define r15 (symbol->gpr64 'r15))

; 32-bit GPRs
(define eax (symbol->gpr32 'eax))
(define ecx (symbol->gpr32 'ecx))
(define edx (symbol->gpr32 'edx))
(define ebx (symbol->gpr32 'ebx))
(define esp (symbol->gpr32 'esp))
(define ebp (symbol->gpr32 'ebp))
(define esi (symbol->gpr32 'esi))
(define edi (symbol->gpr32 'edi))

(define cl (symbol->gpr8 'cl))


; displacement

(define (register-indirect-name r)
  (define base (register-name (register-indirect-gpr r)))
  (define disp (register-indirect-disp r))
  (string->symbol
    (if disp
      (format "[~a+~a]" base disp)
      (format "[a]" base))))

(define (register-indirect-encode r)
  (define p (register-encode (register-indirect-gpr r)))
  (define disp (register-indirect-disp r))
  (define suffix null)
  (define mod
    (cond
      [disp
        (set! suffix (core:bitvector->list/le disp))
        (case (core:bv-size disp)
          [(8)  (bv #b01 2)]
          [(32) (bv #b10 2)])]
      [else
        (bv #b00 2)]))
  (list (car p) mod (cdr p) suffix))

; Be defensive about types to work with both 32- and 64-bit memory models.
(define (sign-cast n x)
  (cond
    ; disp can be #f
    [(not x)
      (bv 0 n)]
    [(< n (core:bv-size x))
      (trunc n x)]
    [else
      (sign-extend x (bitvector n))]))

; No support for absolute addressing or SIB yet:
;                   mod r/m
;   [--][--]        00  100
;   disp32          00  101
;   [--][--]+disp8  01  100
;   [--][--]+disp32 10  100
(define (guard-register-indirect gpr disp size name)
  (define (unsupported)
    (core:bug #:msg (format "guard-register-indirect: unsupported addressing: ~a ~a ~a" gpr disp size)))
  (cond
    ; absolute addressing
    [(and (not disp) (member gpr (list rbp ebp)))
      (unsupported)]
    ; SIB
    [(member gpr (list rsp esp))
      (unsupported)])
  (values gpr disp size))

(define (register-indirect-ref cpu r)
  (define mm (cpu-memmgr cpu))
  (define n (core:memmgr-bitwidth mm))
  (define gpr (register-indirect-gpr r))
  (define addr (sign-cast n (register-ref cpu gpr)))
  (define disp (sign-cast n (register-indirect-disp r)))
  (define size (bv (quotient (register-indirect-size r) 8) n))
  (core:memmgr-load mm addr disp size))

(define (register-indirect-set! cpu r v)
  (define mm (cpu-memmgr cpu))
  (define n (core:memmgr-bitwidth mm))
  (define gpr (register-indirect-gpr r))
  (define addr (sign-cast n (register-ref cpu gpr)))
  (define disp (sign-cast n (register-indirect-disp r)))
  (define size (bv (quotient (register-indirect-size r) 8) n))
  (core:memmgr-store! mm addr disp v size))

(struct register-indirect (gpr disp size)
  #:transparent
  #:guard guard-register-indirect
  #:methods gen:register
  [(define (register-size r) (register-indirect-size r))
   (define register-name register-indirect-name)
   (define register-encode register-indirect-encode)
   (define register-ref register-indirect-ref)
   (define register-set! register-indirect-set!)])
