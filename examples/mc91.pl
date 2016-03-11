
mc91(A,B) :-
	A > 100, 
    B = A-10.
mc91(A,B) :-
    A =< 100, 
    C = A+11,
    mc91(C,D),
    mc91(D,B).
    

false :- A =< 100, mc91(A,B), B > 91.
false :- A =< 100, mc91(A,B), B =< 90.