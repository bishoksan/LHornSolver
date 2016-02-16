/*
Takes as input a set of Horn clauses K+1 dim program, invariants generated for a K-dim program  and K, and generates an output file with those invariants inserted
*/

:-module(insertInvKdimNL, _).

:-dynamic(invariant/1).
:-dynamic(disjInvariant/1).
:-dynamic(dimension/1).

:- use_module(linearize).
:- use_module(input_ppl_clausenum).
:- use_module(canonical).
:- use_module(ppl_ops).
:- use_module(setops).
:- use_module(common).

:- use_module(library(terms_vars)).
:- use_module(library(lists)).
:- use_module(library(strings)).


%K is the index for which the invaraint is computed

go2:-
    main(['-prg', '/Users/kafle/Desktop/linearHornSolver/example/floodfill.nts.pl_output/floodfill.nts.pl.1.pl', '-inv', '/Users/kafle/Desktop/linearHornSolver/example/floodfill.nts.pl_output/floodfill.nts.pl.pe.cha.pl', '-k', 0]), nl.
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
    (member(dim(K),Options), saveDim(K);
                write(user_output,'No dimension file given.'),nl(user_output)),
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
recognised_option('-k', dim(R),[R]).
recognised_option('-o', kdimIterated(R),[R]).


saveDim(K):-
    assert(dimension(K)).

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
    retractall(invariant(_)),
    retractall(dimension(_)).

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

removeExistentialVars(H, Body, Body1, BodySat):-
    separate_constraints(Body, Cs, Bs),
    varset(Bs, BsVar),
    varset(H, HVar),
    append(HVar, BsVar, Vars),
    varset(Cs, CsVar),
    setdiff( CsVar,Vars, Evars),
    numbervars((H, Bs, Cs),0,_),
    makePolyhedron(Cs, PCs),
    project(PCs, Evars, PC1),
    (isEmpty(PC1) ->
        BodySat=unsatisfiable,
        Body1=[1=0|Bs]
    ;
        BodySat=satisfiable,
        getConstraint(PC1,Cs1),
        append(Cs1, Bs, Body1)
    ).


% outputs

writeIteratedKDim(S):-
    start_ppl,
    dimension(K),
    writeIteratedKDim1(S, K),
    end_ppl.

/*
if a head of a clause corresponds to the predicate at level K then eliminate it since every call to it will be replaced by its invariant in the nxt level
*/
writeIteratedKDim1(S, K):-
    my_clause(H, Body,_),

        separate_constraints(Body, Cs, Bs),
        plugBodyInvs(Bs, Bs1, K),
        append(Cs, Bs1, Bs2),
        removeExistentialVars(H, Bs2,  Bs3, BodySat),
        (BodySat=satisfiable ->
            writeClauses(H,Bs3,S)
        ;
            true
        ),

    fail.
writeIteratedKDim1(_,_).

plugBodyInvs([], [],_).
plugBodyInvs([B|Bs], [B1|Bs1], K):-
    (invariant((B,B2)) ->
        list2Conj(B2, B1)
    ;
        %if no invariant, do nothing
        %assignFalse(B, K, B1)
        B1=B
    ),
    plugBodyInvs(Bs, Bs1, K).

/*
predicateLevelKandBelow(H, K):-
    functor(H, P,_),
    name(P, N),
    name(K, NK),
    (
        append(N1, [91|R], N)
    ;
        append(N1, [40|R], N)
    ),
    append(NK, _, R).
*/

predicateLevelKandBelow(H, K):-
    functor(H, P,_),
    name(P, N),
    (
        append(N1, [91|R], N), %91 =[
        append(K1, [93|_], R) %93=]
    ;
        append(N1, [40|R], N), %40=(
        append(K1, [41|_], R) %41 =)
    ),
    name(PK1, K1),
    convert2num(PK1, K2),
    convert2num(K, KN),
    %write('---------------------    '), write(K2), nl,
    K2=<KN, K2>=0.

assignFalse(B, K, (1=0)):-
    functor(B, P,_),
    name(P, N),
    name(K, NK),

    (
        append(N1, [91|R], N)
    ;
        append(N1, [40|R], N)
    ),
    %write('R encoding '), write(R),nl,
    append(NK, _, R),
    !.

assignFalse(B, _, B):-!.

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
