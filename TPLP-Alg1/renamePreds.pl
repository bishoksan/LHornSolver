:- module(renamePreds,[main/1]).

:- use_module(library(lists)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(common)).

main([InF,MemoF,OutF]) :-
	open(InF,read,S1),
	load_file(MemoF),
	open(OutF,write,S2),
	translateFile(S1,S2),
	close(S1),
	close(S2).
main([InF,MemoF]) :-
	open(InF,read,S1),
	load_file(MemoF),
	translateFile(S1,user_output),
	close(S1).	
	
	
translateFile(S1,S2) :-
	read(S1,C),
	(
	    C == end_of_file -> true
	;
	    numbervars(C,0,_),
		renamePreds(C,S2),
	    translateFile(S1,S2)
	).

renamePreds((go__0 :- _),_) :- % remove the start clause
	!.
renamePreds((A :- B),S) :-
	tuple2list(B,BL),
	!,
	separate_constraints(BL,Cs,Bs),
	renameAtom(A,A1),
	renameBody(Bs,Bs1),
	writeClause(A1,Cs,Bs1,S).
renamePreds(A,S) :-
	renameAtom(A,A1),
	writeClause(A1,[],[],S),
	!.
renamePreds(D,S) :-
	writeq(S,D),
	write(S,'.'),
	nl(S).

renameAtom(A,A1) :-
	A =.. [F|Xs],
	atom_concat(solveAtom,_,F),
	!,
	my_clause(memo_table(_,solveAtom(H,_,_),A,_),[],_),
	functor(H,P,_),
	A1 =.. [P|Xs].
renameAtom(A,A).
	
renameBody([],[]).
renameBody([B|Bs],[B1|Bs1]) :-
	renameAtom(B,B1),
	renameBody(Bs,Bs1).

tuple2list((A,As),[A|LAs]) :-
	!,
	tuple2list(As,LAs).
tuple2list(A,[A]).

writeClause(H,[],[],S) :-
	!,
	writeClause(H,[],[true],S).
writeClause(H,Cs,Bs,S) :-
	writeq(S,H),
	write(S,' :-'),
	nl(S),
	append(Cs,Bs,Body),
	writeBody(Body,S).
	
writeBody([],S) :-
	write(S,'.'),
	nl(S).
writeBody([B],S) :-
	!,
	write(S,'    '),
	writeq(S,B),
	write(S,'.'),
	nl(S).
writeBody([B|Bs],S) :-
	write(S,'    '),
	writeq(S,B),
	write(S,','),
	nl(S),
	writeBody(Bs,S).
	

