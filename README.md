# LHornSolver

LHornSolver uses an abstraction-refinement algorithm for solving
non-linear Horn clauses using only a linear Horn clause solver.

## Programming 

LHornSolver is written in Ciao and is interfaced with Yices SMT solver
and Parma polyhedra library for handling constraints. LHornSolver uses
several reusable components such as linear Horn clause solver,
dimension bounded program generator, Horn clause linearisers etc.

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) 1.16 or newer (installed
from git repository with `./ciao-boot.sh local-install`)

## Build and installation

You can automatically fetch, build, and install LHornSolver using:

```sh
ciao get github.com/bishoksan/LHornSolver
```

The following dependendencies (including third-party code) will be
installed automatically:

1. [Ciao bindings](https://github.com/ciao-lang/ciao_ppl) for
   [Parma Polyhedra Library](https://bugseng.com/products/ppl/)
   (`ciao get ciao_ppl`)
2. [Ciao bindings](https://github.com/ciao-lang/ciao_yices) for
   [Yices SMT solver](https://yices.csl.sri.com/) (`ciao get
   github.com/ciao-lang/ciao_yices`)
3. Partial evaluator [Logen](https://github.com/leuschel/logen)
   (install the Ciao port with `ciao get github.com/jfmc/logen`).

All code will be downloaded and built under the first directory
specified in the `CIAOPATH` environment variable or `~/.ciao` by
default.

**For developing** it is recommended to define your own
_workspace directory_ and clone this repository. E.g., `export
CIAOPATH=~/ciao` and update your `PATH` with `eval "$(ciao-env)"`.
The dependencies can be cloned manually or fetched automatically by
calling `ciao fetch` at the source directory.

## Usage

**Usage**: `lhornsolver` \<*input file containing a set of Horn clauses*\>

**Input**: `a set of (non)-linear Horn clauses` written using Prolog
notation: e.g. `h(X):- C, b1(X1),...,bn(Xn).`

**Output**: `solved | unsolved | unknown` if the clauses are solvable
  | unsolvable | unknown.

## Generate a standalone binary distribution

1. `mkdir dist; cd dist`
2. `ciaoc_sdyn ../src/lhornsolver`

This creates a platform specific binary `lhornsolver` at `dist/`
directory, together with the collection of shared libraries for the
dependencies. **Note**: you may need to include a standalone copy of
`logen` binary and related files.

## Programs and benchmarks for TPLP paper "Tree dimension in verification of constrained Horn clauses"

The programs are in directory TPLP-Alg1 and are run using a shell script.  There
is a short file README.txt in that directory giving instructions for use.  The benchmarks
used in that paper are found in directory bench-tplp17-kdim.

