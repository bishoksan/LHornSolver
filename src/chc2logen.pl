:- module(chc2logen,[main/1]).

:- use_module(library(lists)).

main([InF,OutF]) :-
	open(InF,read,S1),
	open(OutF,write,S2),
	translateFile(S1,S2),
	close(S1),
	close(S2).
main([InF]) :-
	open(InF,read,S1),
	translateFile(S1,user_output),
	close(S1).	
	

translateFile(S1,S2) :-
	read(S1,C),
	(
	    C == end_of_file -> true
	;
	    numbervars(C,0,_),
		writeHornClause(C,S2),
	    translateFile(S1,S2)
	).

writeHornClause((A :- B),S) :-
	!,
	tuple2list(B,BL),
	separate_constraints(BL,Cs,Bs),
	writeq(S,logen(hornClause/2,hornClause(A,Cs,Bs))),
	write(S,'.'),
	nl(S).

writeHornClause(A,S) :-
	writeq(S,logen(hornClause/2,hornClause(A,[],[]))),
	write(S,'.'),
	nl(S),
	!.
writeHornClause((:- _),_).

tuple2list((A,As),[A|LAs]) :-
	!,
	tuple2list(As,LAs).
tuple2list(A,[A]).

separate_constraints([],[],[]).
separate_constraints([B|Bs],[B|Cs],Ds) :-
	constraint(B),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).
	
constraint(_ = _).
constraint(_ < _).
constraint(_ > _).
constraint(_ =< _).
constraint(_ >= _).
constraint(_ is _).
constraint(_ =:= _).
constraint(_\==_).
constraint(_=\=_).
constraint(true).
constraint(fail).
