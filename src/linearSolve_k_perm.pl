go(K) :-
	getAtom(A),
	solve([A],K,1).
solve([G|Gs],K,L) :-
	hornClause(G,Cs,B),
	solveConstraints(Cs),
	length(B,L1),
	L2 is L1+L-1,
	L2 =< K,
	perm(B,B1),
	append(B1,Gs,Gs1),
	solve(Gs1,K,L2).
solve([],_,_).

solveConstraints([]).
solveConstraints([C|Cs]) :-
	call(C),
	solveConstraints(Cs).
	
length([],0).
length([_|Xs],N) :-
	length(Xs,M),
	N is M+1.

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :-
	append(Xs,Ys,Zs).
	
select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Ys]) :-
	select(X,Xs,Ys).	
	
perm([],[]).
perm(Xs,[Y|Ys]) :-
	select(Y,Xs,Xs1),
	perm(Xs1,Ys).

getAtom(A) :-
    hornClause(A1,Cs,B),
    functor(A1,P,N),
    functor(A,P,N).

    

	
