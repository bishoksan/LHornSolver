# LHornSolver

LHornSolver uses an abstraction-refinement algorithm for solving
non-linear Horn clauses using only a linear Horn clause solver.

## Programming 

LHornSolver is written in Ciao and is interfaced with Yices SMT solver
and Parma polyhedra library for handling constraints. LHornSolver uses
several reusable components such as linear Horn clause solver,
dimension bounded program generator, Horn clause linearisers etc.

## Requirements

1. [Ciao](https://github.com/ciao-lang/ciao) 1.16 or newer
   (installed from git repository with `./ciao-boot.sh local-install`)
2. [Ciao bindings](https://github.com/ciao-lang/ciao_ppl) for
   [Parma Polyhedra Library](https://bugseng.com/products/ppl/)
   (`ciao get ciao_ppl --ciao_ppl:enabled=yes --ciao_ppl:auto_install=yes`)
3. [Ciao bindings](https://github.com/jfmc/ciao_yices) for
   [Yices SMT solver](https://yices.csl.sri.com/) (`ciao get
   github.com/jfmc/ciao_yices`)
4. Partial evaluator [Logen](https://github.com/leuschel/logen)
   (install the Ciao port with `ciao get github.com/jfmc/logen`).

## Build and installation

You can automatically fetch, build, and install LHornSolver using:

```sh
ciao get github.com/bishoksan/LHornSolver
```

All code will be downloaded and built under the first directory
specified in the `CIAOPATH` environment variable or `~/.ciao` by
default.

**For developing** LHornSolver it is recommended to define your own
_workspace directory_ and clone this repository. E.g., `export
CIAOPATH=~/ciao` and update your `PATH` with `eval "$(ciao-env)"`.
The dependencies can be cloned manually or fetched automatically by
calling `ciao fetch` at the LHornSolver source directory.

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

