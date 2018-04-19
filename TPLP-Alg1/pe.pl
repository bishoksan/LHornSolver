% Specialise a program wrt to a goal and a set of properties

:- module(pe,_).

:- use_module(chclibs(builtins)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(common)).
:- use_module(chclibs(linearize)).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(ppl_ops)).
:- include(chclibs(get_options)).

:- dynamic(prop/2).
:- dynamic(peClause/3).

:- data flag/1.



go(F,Q,Props) :-
	pe:main(['-prg',F, '-entry', Q, '-props',Props]).
	
main(ArgV) :-
	cleanup,
	write('Starting ...'),nl,
	get_options(ArgV,Options,_),
	setOptions(Options,File,Goal,OutS),
	load_file(File),
	start_time,
	start_ppl,
	proplist(L,_),
	functor(Goal,P,N),
	pe([(Goal:-[])],L,[version(P/N,[])],AllVersions), % assume that the initial goal is unconstrained
	numberVersions(AllVersions,P/N,1,NVersions),
	end_time(user_output),
	%showTransitions(AllVersions, NVersions,OutS),
	showVersionClauses(AllVersions, NVersions,OutS),

	close(OutS),
	ppl_finalize.
	


recognised_option('-prg',  program(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-props',propFile(R),[R]).
recognised_option('-entry',entry(Q),[Q]).

	
setOptions(Options,File,Goal,OutS) :-
	(member(program(File),Options); 
			write(user_output,'No input file given.'),
			nl(user_output), 
			fail),
	(member(entry(Q),Options), convertQueryString(Q,Goal); 
			write(user_output,'No goal given or invalid goal.'),
			nl(user_output), 
			fail),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output),
	(member(propFile(PFile),Options), readPropFile(PFile); 
			true).
			
convertQueryString(Q,Q1) :-
	open('/tmp/querystring',write,S),
	writeq(S,Q),
	write(S,'.'),
	nl(S),
	close(S),
	open('/tmp/querystring',read,S1),
	read(S1,Q1),
	close(S1),
	system('rm /tmp/querystring').

cleanup :-
	retractall(prop(_,_)),
	retractall(peClause(_,_,_)).
	
pe([(A :- Ids)|Gs],L,Versions0,Versions2) :-
	versionConstraints(A,Ids,L,Cs),
	resultants(A,Cs,Cls),
	versionClauses(Cls,Ids,L,VCls),
	storeVersionClauses(VCls),
	newVersions(VCls,Versions0,Versions1,NewGs,Gs),
	pe(NewGs,L,Versions1,Versions2).
pe([],_,Vs,Vs).

versionConstraints(A,Ids,L,Cs) :-
	functor(A,F,N),
	getIds(F/N,L,HIs),
	selectIds(Ids,HIs,Hs1),
	intersectionConstraints(Hs1,Cs).
	
resultants(A,Cs,Cls) :-
	functor(A,P,N),
	functor(A1,P,N),
	findall((A1 :- B),my_clause(A1,B,_),Cls0),
	feasibleClauses(Cls0,Cs,Cls).
	
feasibleClauses([],_,[]).
feasibleClauses([(A :-B)|Cls0],Cs,[(A :- Cs3,NLCs,Bs)|Cls]) :-
	separate_constraints(B,Cs1,Bs),
	linearConstraints(Cs1,LCs,NLCs),
	append(Cs,LCs,Cs2),
	numbervars((A,B),0,_),
	satisfiable(Cs2,H),
	!,
	getConstraint(H,Cs3),
	feasibleClauses(Cls0,Cs,Cls).
feasibleClauses([_|Cls0],Cs,Cls) :-	
	feasibleClauses(Cls0,Cs,Cls).
	
linearConstraints([],[],[]).
linearConstraints([C|Cs],[C|LCs],NLCs) :-
	linear_constraint(C),
	!,
	linearConstraints(Cs,LCs,NLCs).
linearConstraints([C|Cs],LCs,[C|NLCs]) :-
	linearConstraints(Cs,LCs,NLCs).

versionClauses([],_,_,[]).
versionClauses([(A :- Cs,NLCs,Bs)|Cls],Ids,L,[(atom(A,Ids) :- Cs,NLCs,VBs)|VCls]) :-
	bodyVersions(Bs,Cs,L,VBs),
	versionClauses(Cls,Ids,L,VCls).

bodyVersions([],_,_,[]).
bodyVersions([B|Bs],Cs,L,[atom(B,Ids)|Bs1]) :-
	abstractVersion(B,Cs,L,Ids),
	bodyVersions(Bs,Cs,L,Bs1).
		
abstractVersion(B,Cs,L,Ids) :-
	melt((B,Cs),(B1,Cs1)),
	varset(B1,Xs),
	varset(Cs1,Ys),
	dummyCList(Xs,DCL),
	append(DCL,Cs1,Cs2),
	setdiff(Ys,Xs,Zs),
	numbervars((B1,Cs2),0,_),
	makePolyhedron(Cs2,H),
	project(H,Zs,H1),
	predicate_abstract(B,H1,L,Ids).
	
newVersions([(_ :- _,_,Bs)|VCls],Versions0,Versions2,Gs0,Gs2) :-
	collectVersions(Bs,Versions0,Versions1,Gs0,Gs1),
	newVersions(VCls,Versions1,Versions2,Gs1,Gs2).
newVersions([],Vs,Vs,Gs,Gs).

collectVersions([atom(A,Ids)|Bs],Vs0,Vs1,Gs0,Gs1) :-
	functor(A,P,N),
	member(version(P/N,Ids),Vs0),
	!,
	collectVersions(Bs,Vs0,Vs1,Gs0,Gs1).
collectVersions([atom(A,Ids)|Bs],Vs0,Vs1,Gs0,Gs1) :-
	functor(A,P,N),
	collectVersions(Bs,[version(P/N,Ids)|Vs0],Vs1,Gs0,[(A:-Ids)|Gs1]).
collectVersions([],Vs,Vs,Gs,Gs).

storeVersionClauses([]).
storeVersionClauses([(atom(A,Ids) :- Cs,NLCs,Bs)|VCls]) :-
	append(Cs,NLCs,Cs1),
	assert(peClause(atom(A,Ids),Cs1,Bs)),
	storeVersionClauses(VCls).

readPropFile(PFile) :-
	open(PFile,read,S),
	read(S,C),
	readPropFacts(S,C),
	close(S).
	
readPropFacts(_,end_of_file) :-
	!.
readPropFacts(S,(H:-C)) :-
	varset(H,Xs),
	dummyCList(Xs,DCL),
	append(C,DCL,CsL),
	assert(prop(H,CsL)),
	read(S,C1),
	readPropFacts(S,C1).
	
proplist(L,K) :-
	findall((A,C),(
		prop(A,C),
		numbervars((A,C),0,_)),
	Ps),
	makePropList(Ps,0,K,[],L).
	
makePropList([],K,K,L,L).
makePropList([(A,C)|Ps],K0,K2,L0,L2) :-
	makePolyhedron(C,H),
	functor(A,P,N),
	N>0,
	!,
	addProperty(P/N,H,C,K0,K1,L0,L1),
	makePropList(Ps,K1,K2,L1,L2).
makePropList([_|Ps],K0,K1,L0,L1) :-
	makePropList(Ps,K0,K1,L0,L1).
	



selectIds([Id|Ids],[H-Id|Hs],[H|Hs1]) :-
	!,
	selectIds(Ids,Hs,Hs1).
selectIds([Id|Ids],[_-Id1|Hs],Hs1) :-
	Id @> Id1,
	!,
	selectIds([Id|Ids],Hs,Hs1).
selectIds([_|Ids],Hs,Hs1) :-
	selectIds(Ids,Hs,Hs1).
selectIds([],_,[]).
	
intersectionConstraints([],[]).
intersectionConstraints([H|Hs],Cs) :-
	ppl_Polyhedron_get_minimized_constraints(H,Cs1),
	intersectionConstraints(Hs,Cs2),
	append(Cs1,Cs2,Cs).

predicate_abstract(Head,H,L,Ids) :-
	functor(Head,P,N),
	predAbstract(P/N,H,L,Ids).

predAbstract(P/N,H,Props,Ids) :-
	member(pred(P/N,LPs),Props),
	!,
	selectProps(LPs,H,Ids).
predAbstract(_,_,_,[]).

selectProps([],_,[]).
selectProps([H1-Id|LPs],H0,[Id|Ids]) :-
	contains(H1,H0),
	!,
	selectProps(LPs,H0,Ids).
selectProps([_|LPs],H0,Ids) :-
	selectProps(LPs,H0,Ids).
	
	
showVersionClauses(AllVersions,NVersions,S) :-
	write(S,'% '),
	write(S, AllVersions),
	nl(S),
	showVersionClauses2(NVersions,S).
	
showVersionClauses2(NVersions,S) :-
	peClause(H,Cs,Bs),
	atomRename(H,NVersions,A),
	bodyRename(Bs,NVersions,Bs1),
	append(Cs,Bs1,B),
	list2conj(B,Body),
	writeq(S,A),
	write(S,' :- '),
	writeq(S, Body),
	write(S,'.'),
	nl(S),
	fail.
showVersionClauses2(_,_).

atomRename(atom(A,Ids),NVersions,A1) :-
	functor(A,P,N),
	A =.. [P|Xs],
	member(nversion(P/N,Ids,P1),NVersions),
	A1 =.. [P1|Xs],
	!.
atomRename(atom(A,Ids),_,_) :-
	write('Cannot find version '),
	write(atom(A,Ids)),
	nl,
	fail.
	
bodyRename([],_,[]).
bodyRename([B|Bs],NVersions,[B1|Bs1]) :-
	atomRename(B,NVersions,B1),
	bodyRename(Bs,NVersions,Bs1).
	
	

getIds(P/N,L,HIs) :-
	member(pred(P/N,HIs), L),
	!.
getIds(_,_,[]).

addProperty(P/N,Hp,_,K0,K1,[pred(P/N,Props)|Ps0],[pred(P/N,Props1)|Ps0]) :-
	!,
	addPredProps(Props,Hp,K0,K1,Props1).
addProperty(P/N,Hp,C,K0,K1,[PredProps|Ps0],[PredProps|Ps1]) :-
	addProperty(P/N,Hp,C,K0,K1,Ps0,Ps1).
addProperty(P/N,Hp,_,K0,K1,[],[pred(P/N,[Hp-K0])]) :-
	K1 is K0+1.

addPredProps([],Hp,K0,K1,[Hp-K0]) :-
	K1 is K0+1.
addPredProps([H-Id|Props],Hp,K,K,[H-Id|Props]) :-
	equivalent(H,Hp),
	!.
addPredProps([H-Id|Props],Hp,K0,K1,[H-Id|Props1]) :-
	addPredProps(Props,Hp,K0,K1,Props1).

numberVersions([version(P/N,[])|AllVersions],P/N,K,[nversion(P/N,[],P)|NVersions]) :-
	!, % initial goal not renamed
	numberVersions(AllVersions,P/N,K,NVersions).
numberVersions([version(Q/M,Ids)|AllVersions],P/N,K,[nversion(Q/M,Ids,QK)|NVersions]) :-
	name(K,NK),
	name(Q,QN),
	append(QN,[95|NK],QKN),
	name(QK,QKN),
	K1 is K+1,
	numberVersions(AllVersions,P/N,K1,NVersions).
numberVersions([],_,_,[]).

list2conj([],true) :-
	!.
list2conj([A],A) :-
	!.	
list2conj([A|As],(A,As1)) :-
	list2conj(As,As1).
	
showTransitions(_AllVersions,NVersions,S) :-
	showGlobals(NVersions,S),
	write(S, 'transitions: ['),
	nl(S),
	showTransitions2(NVersions,S).

	
showGlobals(_NVersions,_S).
	
showTransitions2(NVersions,S) :-
	peClause(H,Cs,Bs),
	write(S,'{'),
	atomRename(H,NVersions,A),
	bodyRename(Bs,NVersions,[B1]),
	functor(A,Source,_),
	functor(B1,Target,_),
	write(S,'source: '),
	writeq(S,Source),
	write(S,','),
	nl(S),
	write(S,'target: '),
	writeq(S,Target),
	write(S,','),
	nl(S),
	write(S,'['),
	writeConstraints(S,Cs),
	write(S,']'),
	write(S,'}'),
	nl(S),
	fail.
showTransitions2(_,_).

writeConstraints(S,[C]) :-
	!,
	write(S,C),
	nl(S).
writeConstraints(S,[C|Cs]) :-
	write(S,C),
	write(S,','),
	nl(S),
	writeConstraints(S,Cs).