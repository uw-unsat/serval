#lang racket/base

(require racket/port
         racket/file
         racket/cmdline
         serval/llvm/parse
         serval/llvm/print)

(define extra-requires (make-parameter null))

(define filename
  (command-line #:program "serval-llvm"
                #:multi [("-R" "--extra-require")
                         path
                         "Add additional requires to add to Racket file"
                         (extra-requires (cons path (extra-requires)))]
                #:args ([input "-"])
                input))

(define input-bytes (if (equal? filename "-") (port->bytes) (file->bytes filename #:mode 'binary)))

(define m (bytes->module input-bytes))
(print-module m #:extra-requires (extra-requires))
