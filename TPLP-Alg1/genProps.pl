:- module(genProps,[main/1]).

:- use_module(library(lists)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(common)).

:- dynamic(flag/1).

main([MemoF,K,PropsF]) :-
	main([MemoF,K,PropsF,'-u']).
	
main([MemoF,K,PropsF,Flag]) :-
	retractall(flag(_)),
	(Flag == '-l' -> assert(flag(atleast)); assert(flag(atmost))),
	load_file(MemoF),
	open(PropsF,write,S),
	(number(K) -> KN=K; atom_number(K,KN)),	% convert when reading from command-line
	genProps(KN,S),
	close(S).

genProps(K,S) :-
	findall((H,A,X), (
		my_clause(memo_table(_,solveAtom(H,X,Id),A,_),[],_),
		numbervars((H,X,Id),0,_)),
		Ps),
	makePropFacts(Ps,K,Fs,[]),
	writeFactList(Fs,S).
	
makePropFacts([],_,Fs,Fs).
makePropFacts([(H,A,X)|Ps],K,Fs0,Fs2) :-
	predFacts(K,H,A,X,Fs0,Fs1),
	makePropFacts(Ps,K,Fs1,Fs2).
	
predFacts(K,H,A,X,Fs0,Fs) :-
	(flag(atmost) -> Fs0=[(B :- [(X>=0)])|Fs]; Fs0=Fs),
	K < 0,
	A =.. [_|Xs],
	functor(H,P,_),
	B =.. [P|Xs].
predFacts(K,H,A,X,[(B :- [C1]),(B :- [C2])|Fs0],Fs1) :-
	(flag(atmost) -> C1=(X=<K),C2=(X=K); C1=(X>=K),C2=(X=K)),
	K >= 0,
	A =.. [_|Xs],
	functor(H,P,_),
	B =.. [P|Xs],
	K1 is K-1,
	predFacts(K1,H,A,X,Fs0,Fs1).
	
writeFactList([],_).
writeFactList([F|Fs],S) :-
	writeq(S,F),
	write(S,'.'),
	nl(S),
	writeFactList(Fs,S).