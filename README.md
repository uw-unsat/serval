# Serval

This repository contains the actively developed version of
Serval.  It may be unstable and break backward compatibility
with previous versions of Serval.  We recommend using this version
unless you have a specific need to use an older version.


You can obtain older, stable snapshots of previous versions of Serval
in two ways. Note that these snapshots are not under active development.

- The [SOSP'19 artifact repository](https://github.com/uw-unsat/serval-sosp19)
  contains a version of Serval used to verify the security monitors described
  in the [paper](https://unsat.cs.washington.edu/papers/nelson-serval.pdf).

- The [SOSP'19 tutorial materials](https://github.com/uw-unsat/serval-tutorial-sosp19)
  contains a version of Serval that is used to verify a toy system. This version
  is useful for learning, but we do not recommend using it to verify a real system.

## Installing

To install serval, first install [Racket](https://racket-lang.org), then run

```sh
raco pkg install
```

from the root directory of this repository.