% Generate an FTA from a program.

:- module(makeFTA,_).

:- use_module(chclibs(builtins)).

:- use_module(library(lists)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(common)).


:- dynamic transition/2.

main(ArgV) :-
	cleanup,
	get_options(ArgV,Options,_),
	setOptions(Options,File,KFile,OutS),
	load_file(File),
	makeFTA,
	retractall(my_clause(_,_,_)),
	load_file(KFile),
	makeFTA_K,
	showFTA(OutS),
	close(OutS).
	

	
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
recognised_option('-kprg',  kprogram(R),[R]).
recognised_option('-o',    outputFile(R),[R]).

setOptions(Options,File,KFile,OutS) :-
	(member(program(File),Options); 
			write(user_output,'No input file given.'),
			nl(user_output), 
			fail),
	(member(kprogram(KFile),Options); 
			write(user_output,'No input k-dim file given.'),
			nl(user_output), 
			fail),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output).
			
cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(transition(_,_)).
	
			
makeFTA :-
	my_clause(H,B,Id),
	functor(H,P,_),
	separate_constraints(B,_,Bs),
	getPreds(Bs,Qs),
	LHS =.. [Id|Qs],
	assert(transition(LHS,P)),
	fail.
makeFTA.

getPreds([],[]).
getPreds([B|Bs],[P|Qs]) :-
	functor(B,P,_),
	getPreds(Bs,Qs).
	
makeFTA_K :-
	my_clause(H,B,_),
	numbervars((H,B),0,_),
	functor(H,P,N),
	separate_constraints(B,Cs,Bs),
	getPreds(Bs,Qs),
	arg(N,H,Xn),
	member((Xn=Id),Cs),
	LHS =.. [Id|Qs],
	assert(transition(LHS,P)),
	fail.
makeFTA_K.

showFTA(S) :-
	transition(Left,Right),
	write(S, (Left -> Right)),
	write(S,'.'),
	nl(S),
	fail.
showFTA(_).
	
