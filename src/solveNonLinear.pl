/*
solves a set of non-linear Horn clauses using only a linear solver

same as linearsolve but with abstraction refinement, returns solved or unsolved or unknown (unknown comes as a result of not using a abstraction refinement based linear solver) if terminates
*/

:- module(solveNonLinear, _).


:- use_module(library(format), [format/2, format/3]).
:- use_module(library(pathnames), [path_basename/2, path_concat/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(system_extra), [mktempdir_in_tmp/2, rmtempdir/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(process), [process_call/3]).

:- use_module(linearsolve).
:- use_module(lineariseCHC).
:- use_module(thresholds1, [main/1]).
:- use_module(counterExample, [main/1]).
:- use_module(load_simple).
:- use_module(cpascc).
:- use_module(common).
:- use_module(checkInv, [checkInv/2]).

:-data constrained_fact/2. % (Atom, Constraint)

% main(['example/fib.pl']).
main([InP]):-
    solve(InP, _).

solve(InP, Result):-
    mktempdir_in_tmp('solveNLinear-XXXXXXXX', ResultDir),
    write('initialising ....'), nl,
    initialise(ResultDir, InP, Dim, F_INV, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX, F_LIN),
    write('abstract refine ....'),  nl,
    abstract_refine(InP, F_INV, Dim, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX,  F_LIN, Result),
    %rmtempdir(ResultDir),
    write('the program '), write(InP), write(' '), write(Result), nl.

abstract_refine(InP, F_INV, Dim, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX , PLin, Result):-
    write('linearising ....'), nl,
    linearise(InP, F_INV,Interpreter, Annotation, Dim, PLin),
    write('solving linearly ....'), nl,
    solve_linear(PLin, Status, ResultL, F_WidenPoints, F_Threshold, CExLinear),
    (Status=safe ->
        checkInv(['-prg', InP, '-inv', ResultL], Safety0),
        (Safety0 = safe ->
            Result=solved % a solution of a linear program becomes a solution of the original one
        ;
            K1 is Dim+1,
            read_constrained_facts(ResultL),
            abstract_refine(InP, F_INV, K1, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX, F_LIN, Result)
        )
    ;
    (Status=unsafe ->
        %refinement
        remove_constrained_facts(PLin,  CExLinear),
        (empty_constrained_facts ->
            Result=unsolved  % ResultL is a counterexample
        ;
            abstract_refine(InP, F_INV, Dim, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX, F_LIN, Result)
        )
    ;
        Result=unknown %the linear solver cannot decide
    )
    ).


% Status=(safe, unsafe, unknown)
solve_linear(PLin, Status, F_INV, F_WidenPoints, F_Threshold, CExLinear):-
    verifyCPA(PLin, F_INV, F_WidenPoints, CExLinear, F_Threshold,Status).


read_constrained_facts(FResultL):-
    open(FResultL, read, S),
    read(S, C),
    save_constrained_facts(S,C),
    close(S).


save_constrained_facts(_, end_of_file):-
    !.
save_constrained_facts(S,(H:-B)):-
    assert(constrained_fact(H,B)),
    read(S,C),
    save_constrained_facts(S, C).

empty_constrained_facts:-
    (constrained_fact(_,_)-> true; fail).

remove_constrained_facts(PLin,  CExLinear):-
    retractall(my_clause(_,_,_)),
    load_file(PLin),
    Predset=[],
    predicatesErrorTrace(CExLinear, Predset, ErrorPreds),
    remove_constrained_facts_error_preds(ErrorPreds).

predicatesErrorTrace(CExLinear, Predset, ErrorPreds):-
    CExLinear=..[C|Ts1],
    my_clause(B,_,C),
    functor(B, P,N),
    append(P/N, Predset, Predset1),
    !,
    predicatesErrorTrace(Ts1, Predset1, ErrorPreds).
predicatesErrorTrace(_, ErrorPreds, ErrorPreds).


remove_constrained_facts_error_preds([]).
remove_constrained_facts_error_preds([P/N|Preds]):-
    functor(A, P, N),
    retract(constrained_fact(A, _)),
    remove_constrained_facts_error_preds(Preds).


% ---------------------------------------------------------------------------
% Analysis using CPA, outputs invariats and cex if any and status=(safe, unsafe, unknown)
% ---------------------------------------------------------------------------

verifyCPA(Prog, F_INV, F_WidenPoints, F_Traceterm, F_Threshold,Result) :-
    thresholds1:main(['-prg', Prog, '-o', F_Threshold]),
    cpascc:main(['-prg', Prog, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints, '-threshold', F_Threshold, '-o', F_INV, '-cex', F_Traceterm]),
    counterExample:main([Prog, F_Traceterm, Result]).

% ---------------------------------------------------------------------------

initialise(ResultDir, InP, Dim, F_INV, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX, F_LIN):-
    Dim=0,
    path_basename(InP, F),
    Interpreter='/Users/kafle/Desktop/LHornSolver/src/linearSolveProg_k_perm.pl',
    Annotation= '/Users/kafle/Desktop/LHornSolver/src/linearSolve_k_perm.pl.ann',
    inv_file(ResultDir, F, F_INV),
    wideningPoints_file(ResultDir, F_WidenPoints),
    threshold_file(ResultDir, F_Threshold),
    traceTerm_file(ResultDir, F_CEX),
    lin_file(ResultDir, F, F_LIN).


wideningPoints_file(ResultDir, F_WidenPoints) :-
	path_concat(ResultDir, 'widenpoints', F_WidenPoints).

traceTerm_file(ResultDir, F_Traceterm) :-
	path_concat(ResultDir, 'traceterm.out', F_Traceterm).

threshold_file(ResultDir, F_Threshold) :-
	path_concat(ResultDir, 'wut.props', F_Threshold).


k_prog_file(ResultDir, F, K, Prog) :-
	number_atom(K, Ka),
	atom_concat([F, '.', Ka, '.pl'], FKpl),
	path_concat(ResultDir, FKpl, Prog).

k_prog_file_s(ResultDir, F, K, Prog) :-
	number_atom(K, Ka),
	atom_concat([F, '.', Ka, '-S.pl'], FKpl),
	path_concat(ResultDir, FKpl, Prog).

inv_file(ResultDir, F, F_PE_CHA) :-
	atom_concat(F, '.pe.cha.pl', F_PE_CHA0),
    write(F_PE_CHA0), nl,
	path_concat(ResultDir, F_PE_CHA0, F_PE_CHA).
    %process_call(path('cd'), [ResultDir], []),
    %process_call(path('touch'), [F_PE_CHA], []).

lin_file(ResultDir, F, F_PE_LIN) :-
	atom_concat(F, '.lin', F_PE_LIN0),
	path_concat(ResultDir, F_PE_LIN0, F_PE_LIN).


cleanup:-
 retractall(constrained_fact(_,_)).






