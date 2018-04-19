:- module(props,_).

:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(common)).


:- dynamic(fact/2).
:- dynamic(prop/2).
:- dynamic(propfact/2).

	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	makeFacts,
	start_ppl,
	preds(Ps),
	initProps(Ps),
	genprops,
	showallprops(OutS),
	nl(OutS),
	close(OutS),
	end_ppl.
	
makeFacts :-
	my_clause(H,B,_),
	separate_constraints(B,Cs,Bs),
	assert(fact(H,Cs)),
	member(B1,Bs),
	assert(fact(B1,Cs)),
	fail.
makeFacts.
	
	
setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output)),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output).

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

recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).


cleanup :-
	retractall(fact(_,_)),
	retractall(prop(_,_)),
	retractall(propfact(_,_)),
	retractall(my_clause(_,_,_)).
	
initProps(Ps) :-
	assert_top_values(Ps).
	
preds(Ps) :-
	setof(P/N, [A,B,C]^(my_clause(A,B,C), functor(A,P,N)),Ps),
	!.
preds([]).
	
assert_top_values([]).
assert_top_values([P/N|Ps]) :-
	functor(A,P,N),
	assert(prop(A,[])),
	assert_top_values(Ps).
	
genprops :-
	fact(A,Cs),
	A =.. [_|Xs],
	solve(Xs,Cs,H,Cs1),
	checkAssert(prop(A,Cs1)),
	A =.. [_|Xs],
	functor(A,_,N),
	projectVars(Xs,[],N,H,Cs2),
	append(Cs1,Cs2,Cs3),
	assert_each_atom_prop(Cs3,A),
	fail.
genprops.

solve(Xs,Cs,Hp,Cs1) :-
	linearize(Cs,CsL),
	varset((Xs,CsL),Ys),
	numbervars((Xs:-CsL),0,_),
	satisfiable(CsL,H1),
	length(Ys,N),
	ppl_Polyhedron_add_space_dimensions_and_embed(H1,N),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp),
	getConstraint(Hp,Cs1).

projectVars([],_,_,_,[]).
projectVars([X|Xs],Ys,N,H,Cs) :-
	append(Xs,Ys,Zs),
	copyPolyhedron(H,H1),
	project(H1,Zs,H2), 	% yields a constraint on '$VAR'(0)
	ppl_Polyhedron_add_space_dimensions_and_embed(H2,N),
	mapCoords(H2,[X-'$VAR'(0)]),
	getConstraint(H2,Cs2),
	projectVars(Xs,[X|Ys],N,H,Cs1),
	append(Cs2,Cs1,Cs).
	
	
assert_each_atom_prop([],B) :-
	!,
	checkAssert(prop(B,[])).
assert_each_atom_prop([C],B) :-
	!,
	checkAssert(prop(B,[C])).
assert_each_atom_prop([C|Cs],B) :-
	checkAssert(prop(B,[C])),
	assert_each_atom_prop(Cs,B).

checkAssert(P) :-
	existingProp(P),
	!.
checkAssert(P) :-
	melt(P, Prop),
	assert(Prop).
	
existingProp(prop(B,C)) :-
	prop(B,C).

	    

	
record(Head,H):-
	cond_assert(Head,H).
	
cond_assert(Head,H):-
	\+ alreadyAsserted(Head,H),
	assert(propfact(Head,H)),
	getConstraint(H,C),
	assert(prop(Head,C)).
		
alreadyAsserted(Head,H) :-
	propfact(Head,H1), 
	equivalent(H,H1).


showallfacts(S) :-
	fact(F,H),
	getConstraint(H,C),
	writeq(S,F), 
	write(S,' :- '), 
	write(S,C),
	write(S,'.'),
	nl(S),
	fail.
showallfacts(_).

showallprops(S) :-
	prop(F,C),
	numbervars((F,C),0,_),
	writeq(S,F), 
	write(S,' :- '), 
	write(S,C),
	write(S,'.'),
	nl(S),
	fail.
showallprops(_).

