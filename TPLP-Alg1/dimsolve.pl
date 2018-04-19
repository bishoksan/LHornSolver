go :-
	getAtom(A),
	solveAtom(A,K,_).
	
solveAtom(G,K,Id) :-
	hornClause(G,Cs,Bs,Id),
	solveConstraints(Cs),
	solveConjunction(Bs,K1s),
	dimension(K1s,K).

solveConjunction([],[]).
solveConjunction([G|Gs],[K|Ks]) :-
	solveAtom(G,K,_),
	solveConjunction(Gs,Ks).
	
solveConstraints([]).
solveConstraints([C|Cs]) :-
	call(C),
	solveConstraints(Cs).
	
select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Ys]) :-
	select(X,Xs,Ys).
	
select2(X1,X2,[X1|Xs],Xs1) :-
	select(X2,Xs,Xs1).
select2(X1,X2,[Y|Xs],[Y|Ys]) :-
	select2(X1,X2,Xs,Ys).
	
dimension([],0).
dimension(Ks,K) :- % linear clause
	select(K,Ks,Ks1),
	K >= 0,
	lesserDimension(Ks1,K).
dimension(Ks,K) :- % non-linear clause
	select2(K1,K1,Ks,Ks1), % select two of the dimensions to be equal
	lesserDimension(Ks1,K),
	K1 >= 0,
	K1 is K-1.

lesserDimension([],_).
lesserDimension([K1|Ks],K) :-
	K1+1 =< K,
	K1 >= 0,
	lesserDimension(Ks,K).
	
getAtom(A) :-
    hornClause(A1,Cs,B,_),
    functor(A1,P,N),
    functor(A,P,N).
 
