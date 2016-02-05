:- module(linearsolve, [main/1], []).

% Solves a non-linear Horn clause using a linear solver.
% Input: a set of Horn clauses
% Output: safe if the program is solved else unknown if it is not solved

:- use_module(library(format), [format/2, format/3]).
:- use_module(library(pathnames), [path_basename/2, path_concat/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(system_extra), [mktempdir_in_tmp/2, rmtempdir/1]).

:- use_module(thresholds1, [main/1]).
:- use_module(cpascc, [main/1]).
:- use_module(checkSafety, [checkSafety/2]).
:- use_module(checkInv, [checkInv/2]).
:- use_module(kdim1, [main/1]).
:- use_module(insertInvKdim, [main/1]).
:- use_module(library(system_extra), [mkpath/1]).

% stores output of the tool
logfile('result.txt').

number_atom(N, A) :- number_codes(N, C), atom_codes(A, C).

% ---------------------------------------------------------------------------
% Prog is program name,
% K (atom) is value of K for the K-dim program,
% OutputFile is the output file
% Result is 'safe' or 'otherwise' (unsafe or unknown)

verifyCPA(Prog, OutputFile, K, Result) :-
	format("Computing widening thresholds for specialised program~n", []),
	thresholds1:main(['-prg', Prog, '-o', 'wut.props']),
	format("Analyse specialised program~n", []),
	cpascc:main(['-prg', Prog, '-withwut', 'bounded', '-wfunc', 'h79', '-o', OutputFile]),
	format("Checking safety~n", []),
	number_atom(K, Ka),
	checkSafety([OutputFile, Ka], Result).

% ---------------------------------------------------------------------------
% Prog contains the program,
% Inv contains the invariant,

checkIndInvarint(Prog, Inv, Ret) :-
	% 'safe' = safe inductive invariant
	% 'unknown' = otherwise
	checkInv(['-prg', Prog, '-inv', Inv], Safety0),
	( Safety0 = safe -> Ret = safe
	; Ret = unknown
	).

% ---------------------------------------------------------------------------
% Prog contains the program,
% K1 has the value of K,
% OutputFile is the K-dim program,

generateKdimProgram(Prog, K1, OutputFile) :-
	number_atom(K1, K1a),
	kdim1:main(['-prg', Prog, '-k', K1a, '-o', OutputFile]).

% ---------------------------------------------------------------------------
% Prog contains the program,
% Inv has the value of K,
% K is the K-dim program,

insertInvariants(Prog, Inv, K, OutputFile) :-
	number_atom(K, Ka),
	insertInvKdim:main(['-prg', Prog, '-inv', Inv, '-k', Ka, '-o', OutputFile]).

% ---------------------------------------------------------------------------
% the main program starts here


%    main(['../example/fib.pl']).


main([Prog1]) :- !,
	linearsolve(Prog1).
main(_) :-
	format(user_error, "Usage: linearsolve <prog>~n~n", []).

linearsolve(Prog1) :-
	logfile(LogFile),
	open(LogFile, append, LogS),
	% create a temporary directory
	mktempdir_in_tmp('linearsolve-XXXXXXXX', ResultDir),
    %atom_concat(Prog1, '_output', ResultDir),
    %mkpath(ResultDir),
	%
	path_basename(Prog1, F),
	K = 0,
    format(LogS, "~w~n", [F]),
	%
	statistics(runtime,[START|_]),
	%
	% format(LogS, "generating ~w-dim program~n", [K]),
	k_prog_file(ResultDir, F, K, Prog),
	number_atom(K, Ka),
	kdim1:main(['-prg', Prog1, '-k', Ka, '-o', Prog]),
	format("The progr is ~w~n", [Prog]),
	%
	loop(LogS, ResultDir, F, K, K2, Prog),
	%
	rmtempdir(ResultDir),
	%
	statistics(runtime,[END|_]),
	DIFF is END - START,
	format(LogS, "total time: ~w~n", [DIFF]),
	format(LogS, "DIMENSION = ~w~n", [K2]),
	format(LogS, "#####################################################################~n", []),
	close(LogS).

loop(LogS, ResultDir, F, K, K2, Prog) :-
	pe_file(ResultDir, F, F_PE_CHA),
	verifyCPA(Prog, F_PE_CHA, K, Ret1),
	( Ret1 = 'otherwise' ->
        K2=K,
	    format(LogS, "the  program maybe unsolved: unknown~n", [])
	; % verifyCPA procedure returned safe to a K-dim program and returned a solution, so proceed to check it against the original program
	  % format(LogS, "checking inductive invariant with ~w-dim invariants~n", [K]),
	  checkIndInvarint(Prog, F_PE_CHA, Ret2),
          ( Ret2 = safe -> % is inductive invariant
             K2=K,
              format(LogS, "the  program is solved~n", [])
	      % format(LogS, "the  inductive inv. are: ~n", []),
              % cat $resultdir/$f.pe.cha.pl >>$logFile
	  ; % not inductive invariant
            % generate K+1 dim program, insert invariant and go back to repeat
            K1 is K + 1,
            format("generating ~w-dim program~n", [K1]),
	    k_prog_file(ResultDir, F, K1, ProgK1),
	    k_prog_file_s(ResultDir, F, K1, ProgK1S),
            generateKdimProgram(Prog, K1, ProgK1),
            format("inserting invariants~n", []),
            insertInvariants(ProgK1, F_PE_CHA, K, ProgK1S),
	    loop(LogS, ResultDir, F, K1, K2, ProgK1S)
	  )
	).

k_prog_file(ResultDir, F, K, Prog) :-
	number_atom(K, Ka),
	atom_concat([F, '.', Ka, '.pl'], FKpl),
	path_concat(ResultDir, FKpl, Prog).

k_prog_file_s(ResultDir, F, K, Prog) :-
	number_atom(K, Ka),
	atom_concat([F, '.', Ka, '-S.pl'], FKpl),
	path_concat(ResultDir, FKpl, Prog).

pe_file(ResultDir, F, F_PE_CHA) :-
	atom_concat(F, '.pe.cha.pl', F_PE_CHA0),
	path_concat(ResultDir, F_PE_CHA0, F_PE_CHA).
