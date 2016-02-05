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
%:- use_module(predicate_map).
:- use_module(common).
:- use_module(chc2logen).


/* */
go:-
    linearise('../example/mc91.pl', [], 'linearSolveProg_k_perm.pl', 'linearSolve_k_perm.pl.ann', 3, '../mc91.lin.pl').

% ---------------------------------------------------------------------------

% (assume that Cogen is in the same directory as the executable)
cogen_executable(Cogen) :-
	current_executable(ExecPath),
    display(ExecPath),
	path_split(ExecPath, ExecDir, _),
	path_concat(ExecDir, 'logen/cogen', Cogen).

% ---------------------------------------------------------------------------



linearise(P, S, Interpreter, Annotation, Dim, PLin):-
    stackSize(P, Dim, Size),
    %pluginSolution(S, P, P1),
    linearisePE(P, Interpreter, Annotation, Size, PLin).

linearisePE(In, Interpreter, Annotation, StackSize, PLin):-
    atom_concat(In, '.logen', InLogen),
    chc2logen:main([In, InLogen]),
	%Interpreter = 'linearSolveProg_k_perm.pl',
	OutAnn = 'linearSolveProg_k_perm.pl.ann',
	%Annotation = 'linearSolve_k_perm.pl.ann',
	copy_file(InLogen, OutAnn, [overwrite]),
	copy_file(Annotation, OutAnn, [append]),
	% logen goal
    number_atom(StackSize, Goal),
	atom_concat(['go(', Goal, ')'], LogenGoal),
	% logen
    %cogen_executable(Logen),
	process_call('logen/cogen', ['--logen_dir', 'logen', '-np', Interpreter, LogenGoal],
	             [stdout(file(PLin))]),
    process_call(path('rm'), [InLogen],[]),
    process_call(path('rm'), [OutAnn],[]).


% formula: Size=(max. nr of body atoms in the program -1)* program_dimension + 1

stackSize(F, Dimension, Size):-
    load_file(F),
    max_nr_of_body_atoms(Index),
    Size is (Index-1)*Dimension + 1,
    print(Size), nl.

max_nr_of_body_atoms(Index):-
    findall(Nr, (my_clause(_,B,_), separate_constraints(B, _, Bs), length(Bs, Nr)), SizeList),
    max_member(SizeList, Index).



max_member([X], X).
max_member([X|R], M):-
    !,
    max_member(R, Max),
    max(X,Max, M).

max(X, Y, X):-
    X>=Y,
    !.
max(_, Y, Y).


number_atom(N, A) :- number_codes(N, C), atom_codes(A, C).
