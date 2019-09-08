#lang scribble/manual

@(require scriblib/autobib)
@(provide (all-defined-out))

@(define-cite ~cite citet generate-bibliography #:style number-style)

@(abbreviate-given-names #t)

@(define nelson:hyperkernel
   (make-bib
     #:title @hyperlink["https://unsat.cs.washington.edu/papers/nelson-hyperkernel.pdf"]{Hyperkernel: Push-Button Verification of an OS Kernel}
     #:author (authors "Luke Nelson" "Helgi Sigurbjarnarson" "Kaiyuan Zhang" "Dylan Johnson" "James Bornholt" "Emina Torlak" "Xi Wang")
     #:date 2017
     #:location (proceedings-location "SOSP")))

@(define sigurbjarnarson:nickel
   (make-bib
     #:title @hyperlink["https://unsat.cs.washington.edu/papers/sigurbjarnarson-nickel.pdf"]{Nickel: A Framework for Design and Verification of Information Flow Control Systems}
     #:author (authors "Helgi Sigurbjarnarson" "Luke Nelson" "Bruno Castro-Karney" "James Bornholt" "Emina Torlak" "Xi Wang")
     #:date 2018
     #:location (proceedings-location "OSDI")))
