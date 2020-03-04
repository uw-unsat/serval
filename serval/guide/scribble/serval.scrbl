#lang scribble/manual

@(require (for-label racket))
@(require (for-label (only-in rosette/base/base bitvector bitvector?)
                     rosette/solver/solution))
@(require (for-label serval/lib/core))
@(require "refs.scrbl")

@title{Serval}


Serval@~cite[nelson:serval] is a framework written in Rosette for creating automated
verifiers for low-level systems code.  Serval comes with
verifiers for the RISC-V, x86-32, LLVM, BPF instruction sets.
Serval provides a library for writing system specifications,
such as state-machine refinement and noninterference.

To develop your own tools,
you may want to familiar yourself with the Rosette language.
@hyperlink["http://docs.racket-lang.org/rosette-guide/"]{The Rosette Guide}
is a good starting point.

@section{Installing Serval}

The easiest way to play with Serval is using a Docker image.
Follow the instructions
on the @hyperlink["https://unsat.cs.washington.edu/projects/serval/sosp19-artifact.html"]{SOSP'19 artifact page}.

You may also be interested in the @hyperlink["https://github.com/uw-unsat/serval-tutorial-sosp19"]{SOSP'19 tutorial} on how to verify a toy security monitor using Serval.

Alternatively, you can install @hyperlink["https://github.com/uw-unsat/serval"](actively developed version) of Serval from source using Racket's @tt{raco} tool:

@itemlist[
  @item{Download and install @hyperlink["http://racket-lang.org"]{Racket} (version 7.5 or later).}
  @item{Git clone the source code in the ``serval'' directory.}
  @item{Uninstall any previous versions of Serval:
    @nested{
            @verbatim|{> raco pkg remove serval}|}}
  @item{Install Serval:
    @nested{
            @verbatim|{> cd serval
                       > raco pkg install}|}}]

@section{References}

@defmodule[serval/lib/core #:use-sources (serval/lib/core)]

Serval provides a library for writing verifiers and specifications.
This includes support for bug reporting, overflow checking for bitvectors,
a memory model for low-level systems, symbolic optimizations, and common specifications.

@subsection{Bug reporting}

@defproc[(bug-on [cond boolean?] [#:key key any/c #f] [#:dbg dbg any/c #f] [#:msg msg string? "Unknown bug-on"]) void?]{
Inserts a check that @racket[cond] must be false under the current path condition,
using @racket[key] and @racket[dbg] to provide optional information
on bug type and location.
This is useful for a verifier to add checks for undefined behavior,
}

@defproc[(bug [#:key key any/c #f] [#:dbg dbg any/c #f] [#:msg msg string? "Unknown bug"]) void?]{
Equivalent to @racket[(bug-on #t)].
}

@defproc[(bug-clear!) void?]{
Clears checks produced by @racket[bug-on].
}

@defproc[(bug-format [data dict?] [sol sat?]) string?]{
Formats a bug report for data produced by @racket[bug-on] using a satisfiable solution.
}

@subsection{Overflow checking}

@defproc*[([(bvsadd-overflow? [x (bitvector n)] [y (bitvector n)] [carry boolean? #f]) boolean?]
           [(bvuadd-overflow? [x (bitvector n)] [y (bitvector n)] [carry boolean? #f]) boolean?])]{
Return whether signed or unsigned addition overflow occurs.
}

@defproc*[([(bvssub-overflow? [x (bitvector n)] [y (bitvector n)] [borrow boolean? #f]) boolean?]
           [(bvusub-overflow? [x (bitvector n)] [y (bitvector n)] [borrow boolean? #f]) boolean?])]{
Return whether signed or unsigned subtraction overflow occurs.
}

@defproc*[([(bvsmul-overflow? [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvumul-overflow? [x (bitvector n)] [y (bitvector n)]) boolean?])]{
Return whether signed or unsigned multiplication overflow occurs.
}

@defproc[(bvsdiv-overflow? [x (bitvector n)] [y (bitvector n)]) boolean?]{
Returns whether signed division overflow occurs.
Note that unsigned division cannot overflow.
}

@subsection{Memory model}

Serval provides a memory model suitable for low-level systems.
It represents memory as a set of disjoint blocks.
Each block may be an array, a struct, or a cell.
Recursively, an array is a list of blocks
of the same types; a struct is a list of named fields, where each
field may further refer to a block.  A cell is a leaf node: it is
represented using an uninterpreted function.

To read and write with an offset into a block, one first invokes
@racket[mblock-path] to convert an offset to a list of indices, or a @italic{path};
@racket[mblock-iload] and @racket[mblock-istore!] use a path to access the leaf cell.

@defthing[gen:mblock mblock?]{
  A generic interface that specifies the procedures provided by a block.
}

@defproc[(mblock? [v any/c]) boolean?]{
Returns true if @racket[v] is a block that implements the @racket[gen:mblock] interface.}

@defproc[(mblock-size [mblock mblock?]) integer?]{
Returns the size of a given block in bytes.
}

@defproc[(mblock-inbounds? [mblock mblock?] [offset (bitvector n)] [size (bitvector n)]) boolean?]{
Returns whether an offset and a size are within the bounds of a given block.
}

@defproc[(mblock-path [mblock mblock?] [offset (bitvector n)] [size (bitvector n)]) list?]{
Resolves an offset and size to a path for a given block.
}

@defproc[(mblock-iload [mblock mblock?] [path list]) any]{
Returns the value at the given path of a memory block.
}

@defproc[(mblock-istore! [mblock mblock?] [value any/c] [path list]) void?]{
Stores the value at the given path of a memory block.
}

@defstruct[mregion ([start integer?] [end integer?] [name symbol?] [block mblock?])
           #:transparent #:omit-constructor]{
Represents a memory block with information about its memory location.
}

@defproc[(find-mregion-by-name [lst (listof mregion?)] [name symbol?]) mregion?]{
Returns a region for a given name.
}

@defproc[(find-mregion-by-addr [lst (listof mregion?)] [addr (bitvector n)]) mregion?]{
Returns a region that contains a given address.
}

@subsection{Symbolic optimizations}

The memory model internally invokes symbolic optimizations to simplify
offsets and blocks.
The following are two symbolic optimizations for case splitting.

@defproc[(split-cases [expr any/c] [cases list?] [proc procedure?]) any?]{
Splits an expression for every concrete value given by @racket[cases],
applies every value to @racket[proc], and returns the merged results.}

@defform[(split-pc [struct field] expr ...)]{
Splits the expression for every possible concrete @italic{ite} value and updates the struct field for subsequent evaluation.
}

@subsection{Common specifications}

Serval provides reusable definitions of common specifications,
including refinement, noninterference, and reference counting.

@subsubsection{Refinement}

@defproc[(verify-refinement
    [#:implstate impl-state any/c]
    [#:impl impl-func procedure?]
    [#:specstate spec-state any/c]
    [#:spec spec-func procedure?]
    [#:abs abs-function procedure?]
    [#:ri rep-invariant procedure?]
    [args list? null]
    [ce-handler procedure? (lambda (sol) (void))]) void?]{
Verifies state-machine refinement
for a given implementation state, implementation function,
specification state, specification function,
abstraction function, and representation invariant,
with @racket[args] passed to both specification and implementation functions.
One may optionally provide a counterexample handler when verification fails.

This helper function combines three checks:
that the implementation doesn't trigger undefined behavior,
that the representation invariant holds on the resulting state of running the implementation
if it holds before,
and that the specification and implementation move in lock-step.
}

@subsubsection{Noninterference}

Serval provides the following definitions for unwinding conditions,
a standard approach for proving noninterference.

@defproc[(check-step-consistency
    [#:state-init init-state procedure?]
    [#:state-copy state-copy procedure?]
    [#:unwinding unwinding procedure?]
    [spec procedure?]
    [args list? null]) void?]{
Verifies the @italic{step consistency} property.
Intuitively, giving any two states s1 and s2 that are equivalent
with respect to an @racket[unwinding] relation,
the resulting states after executing @racket[spec] remain equivalent:
@racket[(unwinding s1 s2)] implies @racket[(unwinding (spec s1) (spec s2))].
}

@defproc[(check-weak-step-consistency
    [#:state-init init-state procedure?]
    [#:state-copy state-copy procedure?]
    [#:invariants inv procedure?]
    [#:dom dom procedure?]
    [#:u u any/c]
    [#:unwinding unwinding procedure?]
    [#:flowsto flowsto procedure?]
    [action any/c]
    [spec procedure?]
    [args list? null]) void?]{
Verifies the @italic{weak step consistency} property,
as described in Nickel@~cite[sigurbjarnarson:nickel].
This is a more general version of @italic{step consistency}
for an intransitive information-flow policy.
It is sometimes referred to as ``confidentiality.''
}

@defproc[(check-local-respect
    [#:state-init init-state procedure?]
    [#:state-copy state-copy procedure?]
    [#:invariants inv procedure?]
    [#:dom dom procedure?]
    [#:u u any/c]
    [#:unwinding unwinding procedure?]
    [#:flowsto flowsto procedure?]
    [action any/c]
    [spec procedure?]
    [args list? null]) void?]{
Verifies the local respect property,
as described in Nickel@~cite[sigurbjarnarson:nickel].
Intuitively, it checks that the states before and after an @racket[action]
are equivalent to a user @racket[u] with respect to a policy.
It is sometimes referred to as ``integrity.''
}

@subsubsection{Reference counting}

The following functions support a specification of reference-counting
consistency, which states that the reference count of a shared
@italic{object} (e.g., a file) equals the size of the set of
@italic{owners} (e.g., per-process file descriptors that refers to
the file).

@defproc[(init-refcnt) refcnt?]{
Returns an new reference counter with a zero reference count.
}

@defproc[(make-havoc-refcnt) refcnt?]{
Returns an new reference counter with a symbolic reference count.
}

@defproc*[([(incr-refcnt [refcnt refcnt?] [owner any/c] [object any/c]) refcnt?]
           [(decr-refcnt [refcnt refcnt?] [owner any/c] [object any/c]) refcnt?])]{
Return a new reference counter by increasing or decreasing the reference count by one.
}

@defproc[(refcnt-invariants [refcnt refcnt?]
    [owner-valid? procedure?]
    [object-valid? procedure?]
    [max-refs (bitvector n)]
    [owned-by? procedure?]) boolean?]{
Returns a predicate of reference-counting
consistency, along with necessary invariants.
It uses the encoding as described in Hyperkernel@~cite[nelson:hyperkernel].
}

@(generate-bibliography)
