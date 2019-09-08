#lang racket/base

(require racket/port
         serval/llvm/parse
         serval/llvm/print)

(define m (bytes->module (port->bytes)))
(print-module m)
