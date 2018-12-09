:- module(chc2logen,[main/1],[]).

:- use_module(library(streams)).
:- use_module(library(read)).
:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(chclibs(common), [conj2List/2, constraint/2]).

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
	conj2List(B,BL),
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

% TODO: see separate_constraints/3 and constraint/1 in chclibs
separate_constraints([],[],[]).
separate_constraints([B|Bs],[B|Cs],Ds) :-
	constraint(B, _),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).
	
