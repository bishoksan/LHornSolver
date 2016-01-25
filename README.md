# LHornSolver
LHornSolver incrementally solves non-linear Horn clauses using only a
linear Horn clause solver.

## Programming 
LHornSolver is written in Ciao and is interfaced with Yices SMT solver
and Parma polyhedra library for handling constraints. LHornSolver uses
several reusable components such as linear Horn clause solver,
dimension bounded program generator etc.

## Requirements
1. [Ciao](http://github.com/ciao-lang/ciao) with
   [Parma Polyhedra Library](http://bugseng.com/products/ppl/) support
   (installed with `./ciao-boot.sh local-install
   --contrib:with_ppl=yes --contrib:auto_install_ppl=yes`)
2. Ciao bindings for [Yices SMT solver](http://yices.csl.sri.com/) (`ciao get github.com/jfmc/ciao_yices`)

## Input and output
Input: a set of Horn clauses. They are written using Prolog notation:
e.g. `h(X):- C, b1(X1),...,bn(Xn).` 

Output: A solution if the clauses are solvable using the tool and unknown otherwise.

## How to run
1. `ciaoc src/linearsolve`
2. `src/linearsolve` \<`File containing a set of Horn clauses`\>

## Generate a standalone binary distribution
1. `mkdir dist; cd dist`
2. `ciaoc_sdyn ../src/linearsolve`

This creates a platform specific binary `linearsolve` at `dist/`
directory, together with the collection of shared libraries for the
dependencies.

