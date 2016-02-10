/*
Takes as input a set of Horn clauses K+1 dim program, invariants generated for a K-dim program  and K, and generates an output file with those invariants inserted
*/

:-module(plugin_solution, _).

:-dynamic(invariant/2).
:-dynamic(dimension/1).

:- use_module(linearize).
:- use_module(input_ppl_clausenum).
:- use_module(canonical).
:- use_module(common).
:- use_module(ppl_ops).
:- use_module(setops).

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
                write(user_output,'No dimension  given.'),nl(user_output)),
    (member(invInserted(Output),Options), open(Output, write, S), writeOutputCls(S), close(S);
			writeOutputCls(user_output)).



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
recognised_option('-o', invInserted(R),[R]).


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
    assert(invariant(H,B)),
    read(S,C),
    saveInv(S, C).


cleanup:-
    retractall(my_clause(_,_,_)),
    retractall(invariant(_,_)),
    retractall(dimension(_)).


% outputs

writeOutputCls(S):-
    start_ppl,
    dimension(K),
    writeOutputCls1(S, K),
    end_ppl.

/*
if a head of a clause corresponds to the predicate at level K then eliminate it since every call to it will be replaced by its invariant in the nxt level
*/
writeOutputCls1(S, _):-
    my_clause(H, Body,_),
    functor(H, P,N),
    functor(H1, P,N),
    ((invariant(H1, Bs), H1\==false) -> %invarints exist for this clause, so replace it definition
        list2Conj(Bs, B),
        numbervars((H1, B), 0,_),
         writeInvClauses(H1,B,S)
    ;
        numbervars((H, Body), 0,_),
        writeClauses(H,Body,S)
    ),
    fail.
writeOutputCls1(_,_).


writeInvClauses(H, B,S) :-
	writeq(S,H),
	write(S,' :-'),
	nl(S),
	write(S,B),
	write(S,'.'),
	nl(S).

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

convert2num(A,A) :-
	number(A),
	!.
convert2num(A,A1) :-
	atom(A),
	atom_number(A,A1).