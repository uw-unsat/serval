#lang racket/base

;(require ffi/unsafe
;         ffi/unsafe/alloc
;         racket/port
;         racket/sequence
;         syntax/strip-context
;         "capi/core.rkt"
;         "util.rkt")

; (provide #%module-begin #%datum #%top-interaction #%app define quote)

; (define (llvm-read-syntax path port)
;   (define m (bytes->module (context) (port->bytes port)))

;   (define fns (for/list ([f (in-functions m)]) (read-function f)))

;   (strip-context
;     (quasisyntax
;       (module anything serval/llvm/lang
;         #,@fns
;       ))))

; (provide (rename-out [llvm-read-syntax read-syntax]))

; (define (read-function f)
;   (define bbs (for/list ([bb (in-basic-blocks f)]) (read-basic-block bb)))
;   (with-syntax ([fnname (string->symbol (value-name f))])
;     (quasisyntax
;       (define (fnname) '#,@bbs))))

; (define (read-basic-block bb)
;   (define insns (for/list ([insn (in-instructions bb)]) (read-instruction insn)))

;   (with-syntax ([name (string->symbol (value->string bb))])
;     (quasisyntax
;       (name #,@insns))))

; (define (read-instruction insn)
;   (with-syntax ([s (value->string insn)])
;     (quasisyntax
;       s)))
