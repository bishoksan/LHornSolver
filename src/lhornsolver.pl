/*
solves a set of non-linear Horn clauses using only a linear solver

same as linearsolve but with abstraction refinement, returns solved or unsolved or unknown (unknown comes as a result of not using a abstraction refinement based linear solver) if terminates
*/

:- module(lhornsolver, _).


:- use_module(library(format), [format/2, format/3]).
:- use_module(library(pathnames), [path_basename/2, path_concat/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(system_extra), [mktempdir_in_tmp/2, rmtempdir/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(process), [process_call/3]).

%:- use_module(linearsolve).
:- use_module(lineariseCHC).
:- use_module(thresholds1, [main/1]).
:- use_module(counterExample, [main/1]).
:- use_module(load_simple).
:- use_module(cpascc).
:- use_module(common).
:- use_module(checkInv, [checkInv/2]).
:- use_module(logen_map).

:-data constrained_fact/2. % (Atom, Constraint)


% stores output of the tool
logfile('result.txt').



% main(['../example/mc91.pl']).
main([InP]):-
    solve(InP, _).

solve(InP, Result):-
    logfile(LogFile),
	open(LogFile, append, LogS),
    mktempdir_in_tmp('solveNLinear-XXXXXXXX', ResultDir),
    write('temp dir: '), nl,
    write(ResultDir), nl,
    write('initialising ....'), nl,
     path_basename(InP, Orig_F),
    initialise(ResultDir, Orig_F, Dim, F_INV, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX, F_LIN, F_KDIM, F_KDIM_S,F_LOGEN_MAP),
    write('abstract refine ....'),  nl,
    statistics(runtime,[START|_]),
    abstract_refine(InP, F_INV, Dim, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX,  F_LIN, F_KDIM, F_KDIM_S, F_LOGEN_MAP, Dim2, Result),
    statistics(runtime,[END|_]),
    DIFF is END - START,
    %remove the directory of intermediate files
    rmtempdir(ResultDir),
    printLHornSolverOutput(LogS,Orig_F, Result, Dim2, DIFF),
    close(LogS).
    %write('the program '), write(InP), write(' is '), write(Result), nl.

abstract_refine(InP, F_INV, Dim, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX , PLin, F_KDIM, F_KDIM_S, F_LOGEN_MAP, Dim2, Result):-
    write('Iteration: '), write(Dim), nl,
    write('linearising ....'), nl,
    linearise(InP, F_INV,Interpreter, Annotation, Dim, F_KDIM, F_KDIM_S, PLin),
    write('solving linearly ....'), nl,
    %PLin1='logenmapFile.pl',
    recoverOriginalPred(PLin, Dim, F_LOGEN_MAP),
    (Dim>0 ->
        read_constrained_facts(F_INV) %facts from the previous iteration
    ;
        true
    ),
    solve_linear(F_LOGEN_MAP, Status, F_INV, F_WidenPoints, F_Threshold, F_CEX),
    (Status=safe ->
        write('checking inductiveness ....'), nl,
        checkInv(['-prg', InP, '-inv', F_INV], Safety0),
        (Safety0 = safe ->
            Dim2=Dim,
            Result=solved % a solution of a linear program becomes a solution of the original one
        ;
            K1 is Dim+1,
            abstract_refine(InP, F_INV, K1, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX, PLin, F_KDIM, F_KDIM_S, F_LOGEN_MAP, Dim2, Result)
        )
    ;
    (Status=unsafe ->
        write('efining ...'), nl,

        (empty_constrained_facts ->
            Dim2=Dim,
            Result=unsolved  % the trace in F_CEX is a counterexample since no constrained facts were used
        ;
            read_constrained_facts(F_INV),
            remove_constrained_facts(F_LOGEN_MAP,  F_CEX),
            abstract_refine(InP, F_INV, Dim, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX, PLin, F_KDIM, F_KDIM_S, F_LOGEN_MAP, Dim2, Result)
        )
    ;
        Dim2=Dim,
        Result=unknown %the linear solver cannot decide
    )
    ).


% Status=(safe, unsafe, unknown)
solve_linear(PLin, Status, F_INV, F_WidenPoints, F_Threshold, CExLinear):-
    verifyCPA(PLin, F_INV, F_WidenPoints, CExLinear, F_Threshold,Status).


read_constrained_facts(F_INV):-
    clean_workplace,
    open(F_INV, read, S),
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
    %write(F_Threshold), nl,
    %write('running cpascc......'), nl,
    cpascc:main(['-prg', Prog, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints, '-threshold', F_Threshold, '-o', F_INV, '-cex', F_Traceterm]),
    write('checking safety ......'), nl,
    counterExample:main([Prog, F_Traceterm, Result]),
    write(Result), nl.

% ---------------------------------------------------------------------------

initialise(ResultDir, F, Dim, F_INV, Interpreter, Annotation, F_WidenPoints, F_Threshold, F_CEX, F_LIN, F_KDIM, F_KDIM_S,F_LOGEN_MAP):-
    Dim=0,
    Interpreter='/Users/kafle/Desktop/LHornSolver/src/linearSolveProg_k_perm.pl',
    Annotation= '/Users/kafle/Desktop/LHornSolver/src/linearSolve_k_perm.pl.ann',
    inv_file(ResultDir, F, F_INV),
    wideningPoints_file(ResultDir, F_WidenPoints),
    threshold_file(ResultDir, F_Threshold),
    traceTerm_file(ResultDir, F_CEX),
    lin_file(ResultDir, F, F_LIN),
    k_prog_file(ResultDir, F, F_KDIM),
    k_prog_file_s(ResultDir, F, F_KDIM_S),
    logen_map_file(ResultDir, F, F_LOGEN_MAP).


wideningPoints_file(ResultDir, F_WidenPoints) :-
	path_concat(ResultDir, 'widenpoints', F_WidenPoints).

traceTerm_file(ResultDir, F_Traceterm) :-
	path_concat(ResultDir, 'traceterm.out', F_Traceterm).

threshold_file(ResultDir, F_Threshold) :-
	path_concat(ResultDir, 'wut.props', F_Threshold).


k_prog_file(ResultDir, F, Prog) :-
	atom_concat([F, '.kdim.pl'], FKpl),
	path_concat(ResultDir, FKpl, Prog).

k_prog_file_s(ResultDir, F, Prog) :-
	atom_concat([F, '.kdim.sub.pl'], FKpl),
	path_concat(ResultDir, FKpl, Prog).

inv_file(ResultDir, F, F_PE_CHA) :-
	atom_concat(F, '.pe.cha.pl', F_PE_CHA0),
	path_concat(ResultDir, F_PE_CHA0, F_PE_CHA).

lin_file(ResultDir, F, F_PE_LIN) :-
	atom_concat(F, '.lin', F_PE_LIN0),
	path_concat(ResultDir, F_PE_LIN0, F_PE_LIN).

logen_map_file(ResultDir, F, F_PE_LIN) :-
	atom_concat(F, 'map.logen', F_PE_LIN0),
	path_concat(ResultDir, F_PE_LIN0, F_PE_LIN).


clean_workplace:-
 retractall(constrained_fact(_,_)).

% ---------------------------------------------------------------------------
% printing output of RAHFT
% ---------------------------------------------------------------------------

printLHornSolverOutput(LogS, Prog, Safety, Iteration, Time):-
	format(LogS, 'LHornSolver: {', []),
	format(LogS, 'Program: ~w, ', [Prog]),
	format(LogS, 'Safety: ~w, ', [Safety]),
	format(LogS, 'Iteration: ~w, ', [Iteration]),
	format(LogS, 'Time: ~w millisecs.} ~n', [Time]).






