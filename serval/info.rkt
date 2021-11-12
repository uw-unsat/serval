#lang info

(define collection 'use-pkg-name)

(define scribblings '(("guide/scribble/serval.scrbl" (multi-page) (experimental))))

(define raco-commands
  '(("serval-llvm" serval/bin/serval-llvm "Convert LLVM .ll file to .rkt using Serval" #f)))
