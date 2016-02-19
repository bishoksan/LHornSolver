# LHornSolver

LHornSolver uses an abstraction-refinement algorithm for solving
non-linear Horn clauses using only a linear Horn clause solver.

## Programming 

LHornSolver is written in Ciao and is interfaced with Yices SMT solver
and Parma polyhedra library for handling constraints. LHornSolver uses
several reusable components such as linear Horn clause solver,
dimension bounded program generator, Horn clause linearisers etc.

## Requirements
1. [Ciao](http://github.com/ciao-lang/ciao) with
   [Parma Polyhedra Library](http://bugseng.com/products/ppl/) support
   (installed with `./ciao-boot.sh local-install
   --contrib:with_ppl=yes --contrib:auto_install_ppl=yes`)
2. Ciao bindings for [Yices SMT solver](http://yices.csl.sri.com/)
   (`ciao get github.com/jfmc/ciao_yices`)
3. Partial evaluator [Logen](https://github.com/leuschel/logen)
   (install the Ciao port with `ciao get github.com/jfmc/logen`).

## Build and installation

You can automatically fetch, build, and install LHornSolver using:

```sh
ciao get github.com/bishoksan/LHornSolver
```

This command stores the source and generates the binaries in the Ciao
_workspace directory_. This directory is given by the value of the
`CIAOPATH` environment variable (or `~/.ciao` if unspecified).

Binaries are placed in the `$CIAOPATH/build/bin` directory (or
`~/.ciao/build/bin`). To call `lhornsolver` without specifying its
full path it is recommended to include this directory in your `PATH`:

```sh
export PATH=$CIAOPATH/build/bin:$PATH
# or export PATH=~/.ciao/build/bin:$PATH
```

**For developing** LHornSolver it is recommended to define `CIAOPATH`
(E.g., `~/ciao`) and clone this repository in your workspace.

## Usage

**Usage**: `lhornsolver` \<*input file containing a set of Horn clauses*\>

**Input**: `a set of (non)-linear Horn clauses` written using Prolog
notation: e.g. `h(X):- C, b1(X1),...,bn(Xn).`

**Output**: `solved | unsolved | unknown` if the clauses are solvable
  | unsolvable | unknown.

## Generate a standalone binary distribution

```sh
mkdir dist; cd dist
ciaoc_sdyn ../src/lhornsolver
```

1. `mkdir dist; cd dist`
2. `ciaoc_sdyn ../src/lhornsolver`

This creates a platform specific binary `lhornsolver` at `dist/`
directory, together with the collection of shared libraries for the
dependencies. **Note**: you may need to include a standalone copy of
`logen` binary and related files.

