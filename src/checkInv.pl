/*
Takes as input a set of Horn clauses and invariants generated for a K-dim program and generates an output file
which contains disjunctively  invariants  as assertions together with the initial set of Clauses
*/

:-module(checkInv, [main/1, checkInv/2]).

:-dynamic(invariant/1).
:-dynamic(disjInvariant/1).

:- use_module(linearize).
:- use_module(input_ppl_clausenum).
:- use_module(yices2_sat).
:- use_module(ciao_yices(ciao_yices_2)).
:- use_module(canonical).

:- include(get_options).
:- use_module(common).

:- use_module(library(terms_vars)).
:- use_module(library(lists)).
:- use_module(library(strings)).


go2:-
    main(['-prg', '/Users/kafle/Desktop/mctest/mc91.pl', '-inv', '/Users/kafle/Desktop/mctest/mc91_1.pl.lin.cha.pl']), nl.
/*
    main(['-prg', 'example/mc91.pl', '-inv', 'example/mc910Inv.pl']), nl.
    %main(['-prg', '/Users/kafle/Desktop/linearHornSolver/example/bfprt.nts.pl', '-inv', '/Users/kafle/Desktop/linearHornSolver/example/bfprt.nts.pl_output/bfprt.nts.pl.pe.cha.pl']), nl.
*/

%ArgV = ['-prg', Input, '-inv', InvariantFile]
main(ArgV):-
	checkInv(ArgV, Safety),
	( Safety = safe -> halt(0)
	; halt(1)
	).

% Safety = safe | unknown
checkInv(ArgV, Safety) :-
	cleanup,
	setOptions(ArgV, Safety).

setOptions(ArgV, Safety) :-
	get_options(ArgV,Options,_),
	( member(programO(Input),Options), load_file(Input)
	; write(user_output,'No input file is given.'),nl(user_output)
	),
	( member(inv(PFile),Options), readInvariants(PFile)
	; write(user_output,'No invariant file is given.'),nl(user_output)
	),
	( member(qarmc(Output),Options),
	  open(Output, append, S), printQarmcOutput(S), close(S)
	; true
	),
	( member(smt(OutputSmt),Options), 
	  open(OutputSmt, write, S1),
	  printSmtOutput(S1, Safety), write(Safety), close(S1)
	;
	  printSmtOutput(user_output, Safety), write(Safety), nl
	).



recognised_option('-prg',  programO(R),[R]).
recognised_option('-inv', inv(R),[R]).
recognised_option('-qarmc', qarmc(R),[R]).
recognised_option('-smt', smt(R),[R]).


%assume there is only one trace in the error trace file
readInvariants(PFile):-
    open(PFile, read, S),
    read(S, C),
    saveInv(S,C),
    close(S).


saveInv(_, end_of_file):-
    !.
saveInv(S,(H:-B)):-
    renameToOriginalPred(H, H1),
    assert(invariant((H1,B))),
    read(S,C),
    saveInv(S, C).


renameToOriginalPred(H, H1):-
    functor(H, P,_),
    H=..[_|Arg],
    name(P, HName),
    (
        append(H1Name, [91|_], HName) %ascii of [=91
    ;
        append(H1Name, [40|_], HName) %ascii of (=40 and )=41
    ;
        H1Name=HName
    ),
    name(P1, H1Name),
    H1=..[P1|Arg].

%do not collect false
predicates(Ps):-
    (setof(P/N, [H,C]^(invariant((H,C)), functor(H,P,N), P \== false), Ps)-> true; Ps=[]).

collectDisjInv([]).
collectDisjInv([P/N|L]):-
    functor(H,P,N),
    findall((H,C), invariant((H,C)), Invariants), %invariants is a list of a list, depends on where we get it from
    collectOnlyConstraints(Invariants, H1, Constraints),
    saveDisjInv(H1, Constraints),
    collectDisjInv(L).

collectOnlyConstraints([], _, []).
collectOnlyConstraints([(H,I)|Invs], H, [I|Cs]):-
    !,
    collectOnlyConstraints(Invs, H, Cs).

%at least one element will be there, it is guranteed since H comes from invariant
saveDisjInv(H, Invariants):-
    listofList2Disj(Invariants, DisjInvariants),
    %melt((H, DisjInvariants), (H1, DisjInvariants1)),
    assert(disjInvariant((H, [DisjInvariants]))).



cleanup:-
    retractall(my_clause(_,_,_)),
    retractall(invariant(_)),
    retractall(disjInvariant(_)).


formDisjInv:-
    predicates(Ps),
    collectDisjInv(Ps).


collectBodyFormula([], []).
collectBodyFormula([B|Bs], Interpolants):-
    (disjInvariant((B,I1)) ->
        I=I1
    ;
        I=[false]
    ),
    collectBodyFormula(Bs, BIs),
    append(I, BIs, Interpolants).

getHeadFormula(B, HF):-
    (disjInvariant((B, HF))->
        true
    ;
        HF=[false]
    ).

getNegHeadFormula(B, HF1):-
    (disjInvariant((B, [HF]))->
        HF1=[neg(HF)]
    ;
        HF1=[true]
    ).


formulaCls([],F, F).

formulaCls([CId|Cls],F, Formula):-
    my_clause(H, B,CId),
    separate_constraints(B, Cs, Bs),
    collectBodyFormula(Bs, BF),
    append(Cs, BF, BFs),
    getNegHeadFormula(H, HF),
    append(BFs, HF, Formula1),
    formulaCls(Cls,[(Formula1;F)], Formula).
    %formulaCls(Cls,[(BFs -> HF)|F], Formula).

clauses(Cls):-
    findall(Id, my_clause(_,_,Id), Cls).

makeRealVars([], []).
makeRealVars([V|Vs], [(V,int)|VReals]):-
    makeRealVars(Vs, VReals).


% outputs

printSmtOutput(_, Safety):-
    formDisjInv,
    clauses(Cls),
    formulaCls(Cls, [false], Formula),
    varset(Formula, Vs),
    numbervars(Formula, 0, _),
    makeRealVars(Vs, VReals),
    %write('fromula n smt '), write(Formula), write(' '),nl,
    %expr2yices(Formula, SmtFormula),
    yices_init,
    (yices_unsat(Formula,VReals)->
        %the original program is safe, there is no interpretation of the predicates which can satistify all clauses
        Safety=safe
        %write(S, ' % The safe inductive invariants are '),
        %nl(S),
        %writeInvs(S),
        %write(S, ' % for the program '), nl(S)
        %writeCls(S),
    ;
        Safety=unknown
        %write(S, 'The following is not an inductive invariant '),
        %nl(S),
        %writeInvs(S)
        %write(S, ' % for the program '), nl(S)
        %writeCls(S),
    ),
    yices_exit.
    %write_string(SmtFormula).

printQarmcOutput(S):-
    formDisjInv,
    writeInvs(S),
    writeCls(S).

writeInvs(S):-
    disjInvariant((H,[I])),
    numbervars((H,I), 0, _),
    write(S, H),
    write(S, ' =:= '),
    write(S, I),
    write(S, '.'),
    nl(S),
    fail.
writeInvs(S):-
    nl(S).

writeCls(S):-
    my_clause(H, Body,_),
    numbervars((H,Body), 0, _),
    writeClauses(H,Body,S),
    fail.
writeCls(_).

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