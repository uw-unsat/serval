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

@(define nelson:serval
   (make-bib
     #:title @hyperlink["https://unsat.cs.washington.edu/papers/nelson-serval.pdf"]{Scaling symbolic evaluation for automated verification of systems code with Serval}
     #:author (authors "Luke Nelson" "James Bornholt" "Ronghui Gu" "Andrew Baumann" "Emina Torlak" "Xi Wang")
     #:date 2019
     #:location (proceedings-location "SOSP")))

@(define sigurbjarnarson:nickel
   (make-bib
     #:title @hyperlink["https://unsat.cs.washington.edu/papers/sigurbjarnarson-nickel.pdf"]{Nickel: A Framework for Design and Verification of Information Flow Control Systems}
     #:author (authors "Helgi Sigurbjarnarson" "Luke Nelson" "Bruno Castro-Karney" "James Bornholt" "Emina Torlak" "Xi Wang")
     #:date 2018
     #:location (proceedings-location "OSDI")))
