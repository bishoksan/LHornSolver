:- module(lineariseCHC, _).
/*
the input is a kdim program P, a set of solution for P, and an index which is an upper bound of the stack size used by the partial evaluator while linearising

The algorithm is presented in the paper: Solving non-linear Horn clauses using a linear solver

*/

:- use_module(library(lists)).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(system), [copy_file/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_basename/2, path_concat/3, path_split/3]).
:- use_module(load_simple).
:- use_module(kdim1).
:- use_module(common).
:- use_module(chc2logen).
:- use_module(linearsolve).
:- use_module(plugin_solution, [main/1]).


/*
go:-
    linearise('../example/mc91.pl', [], 'linearSolveProg_k_perm.pl', 'linearSolve_k_perm.pl.ann', 0, '../mc91.lin.pl').
*/


% ---------------------------------------------------------------------------

% (assume that Cogen is in the same directory as the executable)
cogen_executable(Cogen) :-
	current_executable(ExecPath),
    display(ExecPath),
	path_split(ExecPath, ExecDir, _),
	path_concat(ExecDir, 'logen/cogen', Cogen).

% ---------------------------------------------------------------------------



linearise(P, S, Interpreter, Annotation, Dim, F_KDIM, F_KDIM_S, PLin):-
    number_atom(Dim, Ka),
    %KdimProg='kdimProg.pl',
    write('generating k-dim program '), nl,
    kdim1:main(['-prg', P, '-k', Ka, '-o', F_KDIM]),
    (Dim=0 ->
        linearisePE(F_KDIM, Interpreter, Annotation, 1, PLin)
    ;
        stackSize(P, Dim, Size),
        write('plugin solution  '),nl,
        %P1='kdimProgInsInv.pl',
        pluginSolution(S, F_KDIM, Dim, F_KDIM_S),
        write('linearise PE '), nl,
        linearisePE(F_KDIM_S, Interpreter, Annotation, Size, PLin)
    ).

pluginSolution(Inv, Prog, K,  P1):-
    number_atom(K, Ka),
	plugin_solution:main(['-prg', Prog, '-inv', Inv, '-k', Ka, '-o', P1]).

linearisePE(In, Interpreter, Annotation, StackSize, PLin):-
    atom_concat(In, '.logen', InLogen),
    chc2logen:main([In, InLogen]),
	OutAnn = '/Users/kafle/Desktop/LHornSolver/src/linearSolveProg_k_perm.pl.ann',
	copy_file(InLogen, OutAnn, [overwrite]),
	copy_file(Annotation, OutAnn, [append]),
	% logen goal
    number_atom(StackSize, Goal),
	atom_concat(['go(', Goal, ')'], LogenGoal),
	% logen
    %cogen_executable(Logen),
    %write(PLin), nl,
    %write(LogenGoal ), nl,
	process_call('logen/cogen', ['--logen_dir', '/Users/kafle/Desktop/LHornSolver/src/logen', '-np', Interpreter, LogenGoal],
	             [stdout(file(PLin))]),
    process_call(path('rm'), [InLogen],[]),
    process_call(path('rm'), [OutAnn],[]).


% formula: Size=(max. nr of body atoms in the program -1)* program_dimension + 1

stackSize(F, Dimension, Size):-
    load_file(F),
    max_nr_of_body_atoms(Index),
    Size is (Index-1)*Dimension + 1.
    %print(Size), nl.

max_nr_of_body_atoms(Index):-
    findall(Nr, (my_clause(_,B,_), separate_constraints(B, _, Bs), length(Bs, Nr)), SizeList),
    max_member(SizeList, Index).