:- module(chc2logen,[main/1]).

:- use_module(library(lists)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(common)).

main([InF,OutF]) :-
	load_file(InF),
	open(OutF,write,S2),
	translateFile(S2),
	close(S2).
main([InF]) :-
	load_file(InF),
	translateFile(user_output).	
	

translateFile(S) :-
	my_clause(H,B,Id),
	separate_constraints(B,Cs,Bs),
	numbervars((H,B),0,_),
	writeq(S,logen(hornClause/2,hornClause(H,Cs,Bs,Id))),
	write(S,'.'),
	nl(S),
	fail.
translateFile(_).



