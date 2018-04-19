:- module(fta2chc,_).

:- use_module(chclibs(builtins)).

:- use_module(library(lists)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(common)).

:- dynamic transition/3.
:- dynamic new_clause/2.
:- dynamic statePred/3.
:- dynamic nameCounter/1.

main(ArgV) :-
	cleanup,
	get_options(ArgV,Options,_),
	setOptions(Options,File,FTA,OutS),
	load_file(File),
	load_fta(FTA,Trs),
	makeClauses(Trs),
	makeExtraClauses,
	writeClauses(OutS),
	close(OutS).

go4(InFile, FTA, OFile):-
    main(['-prg', InFile, '-fta', FTA, '-o', OFile]).

	
% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->
	  ( append(Values, Rest, T),
	    RT = Rest,
	    Options = [Opt|OT], Args = AT
	  )
   ;
	  (
	    Options = OT,	Args = [X|AT],
	    RT = T
	  )
   ),
   get_options(RT,OT,AT).

recognised_option('-prg',  program(R),[R]).
recognised_option('-dfta',  dftaFile(R),[R]).
recognised_option('-o',    outputFile(R),[R]).


setOptions(Options,File,FTA,OutS) :-
	(member(program(File),Options); 
			write(user_output,'No input file given.'),
			nl(user_output), 
			fail),
	(member(dftaFile(FTA),Options); 
			write(user_output,'No FTA file given.'),
			nl(user_output), 
			fail),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output).
			
cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(new_clause(_,_)),
	retractall(transition(_,_,_)),
	retractall(nameCounter(_)),
	retractall(statePred(_,_,_)),
	assert(nameCounter(0)).



makeClauses([transition(Left,Right,C)|Trs]) :-
	my_clause(H,B,C),
	separate_constraints(B,Cs,Bs),
	renameHead(H,Right,H1),
	renameBody(Bs,Left,Bs1),
	append(Cs,Bs1,Bs2),
	assert(new_clause(H1,Bs2)),
	makeClauses(Trs).
makeClauses([]).

makeExtraClauses :-
	statePred(Qs,N,Q),
	productState(Qs),
	epsilonClauses(Qs,N,Q),
	fail.
makeExtraClauses.



productState([[_|_]|_]).

epsilonClauses([],_,_).
epsilonClauses([Q|Qs],N,P) :-
	statePred(Q,N,Q1),
	functor(Head,P,N),
	Head =.. [P|Xs],
	Body =.. [Q1|Xs],
	assert(new_clause(Head,[Body])),
	epsilonClauses(Qs,N,P).

renameHead(H,state(Right,P/N),H1) :-
	getStateName(Right,N,P,Q),
	H =..[_|Xs],
	H1=..[Q|Xs].
	
getStateName(Right,N,_,Q) :-
	statePred(Right,N,Q),  	% check for existing name
	!.
getStateName([Q],N,_,Q) :-
	atom(Q),
	!,
	assert(statePred([Q],N,Q)).
getStateName(Qs,N,P,PK) :-
	getCounter(K),
	newPredName(P,K,PK),
	assert(statePred(Qs,N,PK)).
	
renameBody([],[],[]).
renameBody([B|Bs],[state([A|As],P/N)|Args],[B1|Bs1]) :-
	atom(A),
	!,
	getStateName([A|As],N,P,Q),
	B =.. [_|Xs],
	B1=.. [Q|Xs],
	renameBody(Bs,Args,Bs1).
renameBody([B|Bs],[state([A],P/N)|Args],[B1|Bs1]) :- % singleton set
	!,
	getStateName(A,N,P,Q),
	B =.. [_|Xs],
	B1=.. [Q|Xs],
	renameBody(Bs,Args,Bs1).
renameBody([B|Bs],[state(Qs,P/N)|Args],[B1|Bs1]) :-
	!,
	getStateName(Qs,N,P,Q),
	B =.. [_|Xs],
	B1=.. [Q|Xs],
	renameBody(Bs,Args,Bs1).


	
load_fta(File,Trs) :-
	open(File,read,S),
	read(S,Term),
	readTransitions(Term,S,Trs),
	close(S).
	
readTransitions(end_of_file,_,[]).
readTransitions((_ -> R), S,Trs) :-
    finalStateElim(R),
    !,
	read(S,T),
	readTransitions(T,S,Trs).
readTransitions((L -> R), S,[transition(Ys,state(R,P/N),C)|Trs]) :-
	L =.. [C|Xs],
	my_clause(H,B,C),
	separate_constraints(B,_,Bs),
	functor(H,P,N),
	addBodyArities(Bs,Xs,Ys),
	read(S,T),
	readTransitions(T,S,Trs).
	
finalStateElim(R) :- % contains false and some other state
	member(false,R),
	length(R,N),
	N > 1.

addBodyArities([],[],[]).
addBodyArities([B|Bs],[Q|Qs],[state(Q,P/N)|Qs1]) :-
	functor(B,P,N),
	addBodyArities(Bs,Qs,Qs1).
	
newPredName(P,K,PK) :-
	name(P,QQ),
	name(K,KN),
	append(QQ,[95|KN],QKN),
	name(PK,QKN).
	
getCounter(K) :-
	retract(nameCounter(K)),
	K1 is K+1,
	assert(nameCounter(K1)).
	
writeClauses(S) :-
	new_clause(A,Body),
	numbervars((A,Body),0,_),
	writeq(S,A),
	write(S,' :- '),
	nl(S),
	writeBodyClauses(Body,S),
	write(S,'.'),
	nl(S),
	fail.
writeClauses(_).
	
writeBodyClauses([],S) :-
	write(S,'      '),
	write(S,true).
writeBodyClauses([A],S) :-
	!,
	write(S,'      '),
	writeq(S,A).
writeBodyClauses([A|As],S) :-
	write(S,'      '),
	writeq(S,A),
	write(S,','),
	nl(S),
	writeBodyClauses(As,S).
	