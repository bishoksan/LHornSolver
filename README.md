# LHornSolver
LHornSolver incrementally solves non-linear Horn clauses using only a linear Horn clause solver.

## Programming 
LHornSolver is written in Ciao prolog (32 bit version) and is interfaced with Yices SMT solver and Parma polyhedra libray for handling constraints. Any library interfaced with Ciao should also be in 32 bit. LHornSolver uses several reusable components such as linear Horn clause solver, dimension bounded program generator
etc. tied together using a shell script.

## Requirements
1. SMT solver Yices 2.3.1 
2. Parma polyhedra libray

## Input and output:
Input: a set of Horn clauses. They are written using Prolog notation:
e.g. h(X):- C, b1(X1),...,bn(Xn). 

Output: A solution if the clauses are solvable using the tool and unknown otherwise.

## How to run:
1. cd src
2. ciaoc thresholds1; ciaoc cpascc; ciaoc checkSafety; ciaoc checkInv; ciaoc kdim1; ciaoc insertInvKdim 
3. sh linearsolve.sh \<File containing a set of Horn clauses\>

Please edit linearsolve.sh before running the program.