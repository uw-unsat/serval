#lang info

(define collection 'use-pkg-name)

(define scribblings
  '(("guide/scribble/serval.scrbl"
	 (multi-page)
	 (experimental))))

(define raco-commands
  '(("serval"
	 serval/bin/serval
	 "Run serval verification"
	 #f)))

