:- module(counterExample, [checkCounterExample/3, main/1], []).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(terms_vars)).
:- use_module(library(lists)).

:- use_module(linearize).
:- use_module(ppl_ops).
:- use_module(input_ppl_clausenum).
:- use_module(yices2_sat).
:- use_module(ciao_yices(ciao_yices_2)).

:- use_module(common).

main([F, TraceF,Result]) :-
	unsafe(F,TraceF, Result).
	
unsafe(F,PFile, Result) :-
	open(PFile,read,S),
	read(S,C),
	existsCex(S,C,Cex),
	close(S),
	checkCounterExample(Cex,F, Result).
	
existsCex(_,end_of_file,no) :-
	!.
existsCex(_,(cex(Cex)),Cex) :-
	!,
	write(user_output,Cex),
	nl(user_output).
existsCex(_,(counterexample(Cex)),Cex) :-
	!.
	%write(user_output,Cex),
	%nl(user_output).
existsCex(S,_,Cex) :-
	read(S,C1),
	existsCex(S,C1,Cex).
	
checkCounterExample(no,_, Result) :-
	!,
	Result=safe.
checkCounterExample(Cex,F, Result) :-
   %write('cex is '), write(Cex), nl,
	load_file(F),
	( getCeXConstraint([false],[],[Cex], Cs), checkYicesSat(Cs)

	; getCeXConstraint([false_ans],[],[Cex], Cs), checkYicesSat(Cs)
	),
	!,
	Result=unsafe.
checkCounterExample(_,_, Result) :-
	yices_exit,
	Result=spurious.


%the last argument is a set of constraint corresponding to a counterexample T
getCeXConstraint([],Cs,_, Cs).
getCeXConstraint([B|Bs],Cs,[T|Ts], Cs3) :-
	T =..[C|Ts1],
	my_clause(B,Bs1,C),
	separate_constraints(Bs1,Cs1,Bs2),
	append(Bs2,Bs,Bs3),
	append(Cs1,Cs,Cs2),
	append(Ts1,Ts,Ts2),
	getCeXConstraint(Bs3,Cs2,Ts2, Cs3).

checkYicesSat(Formula):-
    varset(Formula, Vs),
    numbervars(Formula, 0, _),
    makeYicesIntVars(Vs, VReals),
    yices_init,
    yices_sat(Formula,VReals),
    yices_exit.



	
