:- module(common, _).


:- use_module(library(lists)).

separate_constraints([],[],[]).
separate_constraints([B|Bs],[C|Cs],Ds) :-
	constraint(B,C),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).


constraint(X=Y, X=Y).
constraint(X=:=Y, X=Y).
constraint(X is Y, X = Y).
constraint(X>Y, X>Y).
constraint(X>=Y, X>=Y).
constraint(X=<Y, X=<Y).
constraint(X<Y, X<Y).

number_atom(N, A) :- number_codes(N, C), atom_codes(A, C).

max_member([X], X).
max_member([X|R], M):-
    !,
    max_member(R, Max),
    max(X,Max, M).

max(X, Y, X):-
    X>=Y,
    !.
max(_, Y, Y).