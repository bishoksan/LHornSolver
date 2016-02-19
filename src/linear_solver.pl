/*
Solves linear Horn clauses
*/

:-module(linear_solver, _).

:-dynamic(invariant/1).
:-dynamic(disjInvariant/1).

:- use_module(linearize).
:- use_module(input_ppl_clausenum).
:- use_module(canonical).
:- use_module(ppl_ops).
:- use_module(setops).

:- use_module(library(terms_vars)).
:- use_module(library(lists)).
:- use_module(library(strings)).



go2:-
    main(['-prg', 'example/fib.pl', '-inv', 'example/fib0Inv.pl']), nl.
    %main(['-prg', 'example/mc91.pl', '-inv', 'example/mc910Inv.pl']), nl.

%ArgV = ['-prg', Input, '-inv', InvariantFile]
main(ArgV):-
    cleanup,
    setOptions(ArgV).

setOptions(ArgV) :-
	get_options(ArgV,Options,_),
	(member(programO(Input),Options), load_file(Input);
			write(user_output,'No input file given.'),nl(user_output)),
    (member(inv(PFile),Options), readInvariants(PFile);
                write(user_output,'No invariant file given.'),nl(user_output)),
    (member(kdimIterated(Output),Options), open(Output, write, S), writeIteratedKDim(S), close(S);
			writeIteratedKDim(user_output)).



% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->
	  ( append(Values, Rest, T),
	    RT = Rest,
	    Options = [Opt|OT], Args = AT
	  )
   ;
	  (
	    Options = OT,	Args = [X|AT],
	    RT = T
	  )
   ),
   get_options(RT,OT,AT).

recognised_option('-prg',  programO(R),[R]).
recognised_option('-inv', inv(R),[R]).
recognised_option('-o', kdimIterated(R),[R]).




%given a non-linear program P, it solves whether it has a solution or not
/*
algorithm

1. Get 0-dim program from P, say P_0.
2. Use linear solver to find if P_0 is solvable.
3. If P_0 is not solvable, emit cEx and return "unsafe".
4. If P_0 is solvable,  get the solution and check if the solution is inductive wrt the original program.
5. If the solution is inductive then return "safe"
6. If the solution is not inductive, then generate 1-dim program.
7. Use the solution from 0-dim program and plug it into the 1-dim program to get a linear version of 1-dim program say P_1L
8. set P_0=P_1L and goto step 2
*/
solve(P, Result):-
    load_file(P),
    kdim(K, P, KP),
    cpa(KP, SolutionKP),
    safe(SolutionKP, K),
    true. % TODO: ???

%assume there is only one trace in the error trace file
readInvariants(PFile):-
    open(PFile, read, S),
    read(S, C),
    saveInv(S,C),
    close(S).


saveInv(_, end_of_file):-
    !.
saveInv(S,(H:-B)):-
    assert(invariant((H,B))),
    read(S,C),
    saveInv(S, C).


cleanup:-
    retractall(my_clause(_,_,_)),
    retractall(invariant(_)).

separate_constraints([],[],[]).
separate_constraints([B|Bs],[C|Cs],Ds) :-
	constraint(B,C),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs1,Ds) :-
	constraint1(B,C),
	!,
    append(C, Cs, Cs1),
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).

list2Conj([], (true)). % meaning true
list2Conj([A], (A)):-
    !.
list2Conj([A|R], (A,R1)):-
    !,
list2Conj(R, R1).

removeExistentialVars(H, Body, Body1):-
    separate_constraints(Body, Cs, Bs),
    varset(Bs, BsVar),
    varset(H, HVar),
    append(HVar, BsVar, Vars),
    varset(Cs, CsVar),
    setdiff( CsVar,Vars, Evars),
    numbervars((H, Bs, Cs),0,_),
    makePolyhedron(Cs, PCs),
    project(PCs, Evars, PC1),
    getConstraint(PC1,Cs1),
    append(Cs1, Bs, Body1).


% outputs

writeIteratedKDim(S):-
    start_ppl,
    writeIteratedKDim1(S),
    end_ppl.

writeIteratedKDim1(S):-
    my_clause(H, Body,_),
    separate_constraints(Body, Cs, Bs),
    plugBodyInvs(Bs, Bs1),
    append(Cs, Bs1, Bs2),
    removeExistentialVars(H, Bs2,  Bs3),
    %numbervars((H,Bs2), 0, _),
    write('body '), write(Bs3), nl,
    writeClauses(H,Bs3,S),
    fail.
writeIteratedKDim1(_).

plugBodyInvs([], []).
plugBodyInvs([B|Bs], [B1|Bs1]):-
    (invariant((B,B2)) ->
        list2Conj(B2, B1)
    ;
        B1=B
    ),
    plugBodyInvs(Bs, Bs1).

writeClauses(H, B,S) :-
	writeq(S,H),
	write(S,' :-'),
	nl(S),
	writeBodyAtoms(S,B),
	write(S,'.'),
	nl(S).
	
writeBodyAtoms(S,[]) :-
	!,
	write(S,'   '),
	write(S,true).
writeBodyAtoms(S,[B]) :-
	!,
	write(S,'   '),
	writeq(S,B).
writeBodyAtoms(S,[B1,B2|Bs]) :-
	write(S,'   '),
	writeq(S,B1),
	write(S,','),
	nl(S),
	writeBodyAtoms(S,[B2|Bs]).