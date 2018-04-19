:- module(counterExample1,_).

:- use_module(chclibs(common)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).
:- use_module(library(terms_vars)).


main([F]) :-
	unsafe(F,'traceterm.out').
	
unsafe(F,PFile) :-
	open(PFile,read,S),
	read(S,C),
	existsCex(S,C,Cex),
	close(S),
	checkCounterExample(Cex,F).
	
existsCex(_,end_of_file,no) :-
	!.
existsCex(_,safe,no) :-
	!.
existsCex(_,(cex(Cex)),Cex) :-
	!.
existsCex(_,(counterexample(Cex)),Cex) :-
	!.
existsCex(S,_,Cex) :-
	read(S,C1),
	existsCex(S,C1,Cex).
	
checkCounterExample(no,_) :-
	!,
	write('Program is safe'),
	nl,
	halt(1).
checkCounterExample(Cex,F) :-
	load_file(F),
	(checkTrace([false],[],[Cex]);
	 checkTrace([false_ans],[],[Cex])
	),
	!,
	yices_exit,
	write('Program is unsafe'),
	nl,
	halt(0).
checkCounterExample(_,_) :-
	yices_exit,
	write('Program might be unsafe'),
	nl,
	nl,
	halt(2).
	
checkTrace(A,_,Trace) :-
	andTreeConstraint(A,Trace,Cs),
	feasible(Cs).

andTreeConstraint([],[],[]) :-
	!.
andTreeConstraint([B|Bs],[T|Ts],Cs) :-
	!,
	andTreeConstraint(B,T,Cs1),
	andTreeConstraint(Bs,Ts,Cs2),
	append(Cs1,Cs2,Cs).
andTreeConstraint(A,Trace,Cs) :-
	Trace =.. [C|Ts],
	my_clause(A,B,C),
	separate_constraints(B,Cs1,Bs),
	andTreeConstraint(Bs,Ts,Cs2),
	append(Cs1,Cs2,Cs).
	
feasible(Cs) :-
	varset(Cs,Vs),
	linearConstraints(Cs,LCs,_),
	numbervars(Vs,0,_),
	yices_init,
	yices_vars(Vs,real,Ws),
	yices_sat(LCs,Ws).
	
linearConstraints([],[],[]).
linearConstraints([C|Cs],[C|LCs],NLCs) :-
	linear_constraint(C),
	!,
	linearConstraints(Cs,LCs,NLCs).
linearConstraints([C|Cs],LCs,[C|NLCs]) :-
	linearConstraints(Cs,LCs,NLCs).
	