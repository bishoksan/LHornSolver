:- module(kdim,[script/0,main/1,go/2,go1/3,go2/2,go3/3]).



:- use_module(library(lists)).

:- use_module(library(sort)).





:- dynamic my_clause/3.

:- dynamic lowerbound/1.



% (c) B. Kafle, J.P. Gallagher, P. Ganty

% Adapted from procedure described in:

% J. Esparza, S. Kiefer, and M. Luttenberger. 

% On fixed point equations over commutative semirings. 

% In STACS 2007, 24th Annual Symposium on Theoretical Aspects of Computer Science, Proceedings, 

% volume 4393 of LNCS, pages 296-307. Springer, 2007.



% For detailed description see following paper:

% Bishoksan Kafle, John P. Gallagher and Pierre Ganty. 

% Decomposition by Tree Dimension in Horn Clause Verification. 

% in Proc. Third International Workshop on Verification and Program Transformation.

% eds. Emanuele De Angelis, Alexei Lisitsa, Andrei P. Nemytykh, Alberto Pettorossi.

% April 2015 (http://refal.botik.ru/vpt/vpt2015/)

% To appear in EPTCS.



% Input:  (for details see below)

% a file containing Horn clauses

% a positive integer k



% Output (default):

% A set of Horn clauses generating the at-most-k-dimensional derivations of the input clauses

% Output (with -lb option): 

% A set of Horn clauses generating the at-least-k+1-dimensional derivations of the input clauses



% Usage 

% go('Tests/mc91.pl',2). 				(write to standard output)

% go1('Tests/mc91.pl',2,'outfile.txt'). (write to named output file)

% go2('Tests/mc91.pl',2). 				(use size argument as lower bound and write to standard output)

% go3('Tests/mc91.pl',2 'output.txt'). 	(use size argument as lower bound and use named output file)



% tested in Ciao Prolog and SWI Prolog

script :-
	current_prolog_flag(argv,Args),
	get_arguments(Args,F,K,OutFile),
	go1(F,K,OutFile).

get_arguments(Args,F,K,OutFile):-
	Args = [F,K,OutFile].

go(F,K) :-

	main(['-prg',F,'-k',K]).

go1(F,K,OutFile) :-

	main(['-prg',F,'-k',K,'-o',OutFile]).

go2(F,K) :-

	main(['-prg',F,'-k',K,'-lb']).

go3(F,K,OutFile) :-

	main(['-prg',F,'-k',K,'-lb','-o',OutFile]).

	

	

% command line usage (if compiled with Ciao Prolog's ciaoc command)

% kdim -prg Tests/mc91.pl -k 2 						(write to standard output)

% kdim -prg Tests/mc91.pl -k 2 -o outfile.txt 		(write to named output file)

% kdim -prg Tests/mc91.pl -k 2 -lb 					(use size argument as lower bound and write to standard output)

% kdim -prg Tests/mc91.pl -k 2 -lb -o outfile.txt 	(use size argument as lower bound and use named output file)





main(ArgV) :-

	cleanup,

	setOptions(ArgV,File,OutS,K),

	load_file(File),

	clauseIds(Ids),

	all_k_dim_clauses(Ids,K,Cls,[]),

	writeClauses(Cls,OutS),

	close(OutS).







setOptions(ArgV,File,OutS,K1) :-

	get_options(ArgV,Options,_),

	(member(programO(File),Options) -> true; 

			write(user_output,'No input file given.'),nl(user_output),fail),

	(member(dimension(K),Options) -> convert2num(K,K1); 

			write(user_output,'No dimension given.'),nl(user_output),fail),

	(member(lowerbound,Options) -> assert(lowerbound(true)); 

			assert(lowerbound(false))),

	(member(outputFile(OutFile),Options) -> open(OutFile,write,OutS); 

				OutS=user_output).

				

convert2num(A,A) :-

	number(A),

	!.

convert2num(A,A1) :-

	atom(A),

	atom_number(A,A1).



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

recognised_option('-k',    dimension(K),[K]).

recognised_option('-lb',   lowerbound,[]).



cleanup :-

	retractall(my_clause(_,_,_)),

	retractall(lowerbound(_)).

	



	

%----------- transform clauses ---------

% for each clause, generate the k-dim clauses

all_k_dim_clauses([Id|Ids],K,Cls0,Cls2) :-

	my_clause(H,B,Id),

	separate_constraints(B,Cs,Bs),

	numbervars((H,B),0,_),

	make_k_dim_clauses(Cs,Bs,H,K,Cls0,Cls1),

	all_k_dim_clauses(Ids,K,Cls1,Cls2).

all_k_dim_clauses([],K,Cls0,Cls2) :-

	allPreds(Ps),

	make_epsilon_clauses(Ps,K,Cls0,Cls1),

	lowerbound(LB),			% optionally, add lower-bound clauses

	make_lowerbound_clauses(LB,Ps,Cls1,Cls2).

	

allPreds(Ps) :-

	setof(P/N,(A,B,C)^

		(my_clause(A,B,C),

		functor(A,P,N)),

	Ps),

	!.

allPreds([]).

	

make_epsilon_clauses([P|Ps],K,Cls0,Cls2) :-	

	p_epsilon_clauses(K,P,Cls0,Cls1),

	make_epsilon_clauses(Ps,K,Cls1,Cls2).

make_epsilon_clauses([],_,Cls,Cls).



p_epsilon_clauses(K,P/N,Cls0,Cls2) :-

	K >= 0,

	!,

	functor(A,P,N),

	numbervars(A,0,_),

	p_epsilon_clauses_loop(K,K,A,Cls0,Cls1),

	K1 is K-1,

	p_epsilon_clauses(K1,P/N,Cls1,Cls2).

p_epsilon_clauses(_,_,Cls,Cls).



p_epsilon_clauses_loop(K,E,A,[(A1 :- [A2])|Cls0],Cls1) :-

	E >= 0,

	dim_atomname(A,'[]',K,A1),

	dim_atomname(A,'()',E,A2),

	E1 is E-1,

	p_epsilon_clauses_loop(K,E1,A,Cls0,Cls1).

p_epsilon_clauses_loop(_,_,_,Cls,Cls).



make_lowerbound_clauses(false,_,Cls,Cls).

make_lowerbound_clauses(true,Ps,Cls0,Cls2) :-

	gen_base_clauses(Ps,Cls0,Cls1),

	clauseIds(Ids),

	gen_orig_clauses(Ids,Cls1,Cls2).

	

gen_base_clauses([P/N|Ps],[(H0 :- [H])|Cls0],Cls1) :-

	functor(H,P,N),

	numbervars(H,0,_),

	dim_atomname(H,'()',0,H0),

	gen_base_clauses(Ps,Cls0,Cls1).

gen_base_clauses([],Cls,Cls).



gen_orig_clauses([Id|Ids],[(H:-B)|Cls0],Cls1) :-

	my_clause(H,B,Id),

	numbervars((H,B),0,_),

	gen_orig_clauses(Ids,Cls0,Cls1).

gen_orig_clauses([],Cls,Cls).



make_k_dim_clauses(Cs,Bs,H,K,Cls0,Cls3) :-

	make_0_dim_clauses(Cs,Bs,H,Cls0,Cls1),

	make_lin_clauses(K,Cs,Bs,H,Cls1,Cls2),

	make_nonlin_clauses(K,Cs,Bs,H,Cls2,Cls3).

	

make_0_dim_clauses(Cs,[],H,[(H1 :- Cs)|Cls0],Cls0) :-

	!,

	dim_atomname(H,'()',0,H1).

make_0_dim_clauses(_,_,_,Cls,Cls).

	

make_lin_clauses(K,Cs,[B],H,[(H1 :- B2)|Cls0],Cls1) :-

	K >= 0,

	!,

	dim_atomname(H,'()',K,H1),

	dim_atomname(B,'()',K,B1),

	append(Cs,[B1],B2),

	K1 is K-1,

	make_lin_clauses(K1,Cs,[B],H,Cls0,Cls1).

make_lin_clauses(_,_,_,_,Cls,Cls).



make_nonlin_clauses(K,Cs,Bs,H,Cls0,Cls3) :-

	K > 0,

	Bs = [_,_|_],

	!,

	dim_atomname(H,'()',K,H1),

	bagof(([B],Bs1),

		select(B,Bs,Bs1),

		Bodies1),

	gen_type1_nonlin_bodies(Bodies1,Cs,K,H1,Cls0,Cls1),

	bagof(([B1,B2],Bs3),(Ds1,Ds2,Ds3)^

		(append(Ds1,[B1|Ds2],Bs),

		select(B2,Ds2,Ds3),

		append(Ds1,Ds3,Bs3)),

		Bodies2),

	gen_type2_nonlin_bodies(Bodies2,Cs,K,H1,Cls1,Cls2),

	K1 is K-1,

	make_nonlin_clauses(K1,Cs,Bs,H,Cls2,Cls3).

make_nonlin_clauses(_,_,_,_,Cls,Cls).



gen_type1_nonlin_bodies([(Bs1,Bs2)|Bodies],Cs,K,H1,[(H1 :- Body)|Cls0],Cls1) :-

	K1 is K-1,

	rename_atoms(Bs1,'()',K,Bs3),

	rename_atoms(Bs2,'[]',K1,Bs4),

	append(Bs3,Bs4,Bs5),

	append(Cs,Bs5,Body),

	gen_type1_nonlin_bodies(Bodies,Cs,K,H1,Cls0,Cls1).

gen_type1_nonlin_bodies([],_,_,_,Cls,Cls).

	

gen_type2_nonlin_bodies([(Bs1,Bs2)|Bodies],Cs,K,H1,[(H1 :- Body)|Cls0],Cls1) :-

	K1 is K-1,

	rename_atoms(Bs1,'()',K1,Bs3),

	rename_atoms(Bs2,'[]',K1,Bs4),

	append(Bs3,Bs4,Bs5),

	append(Cs,Bs5,Body),

	gen_type2_nonlin_bodies(Bodies,Cs,K,H1,Cls0,Cls1).

gen_type2_nonlin_bodies([],_,_,_,Cls,Cls).



rename_atoms([B|Bs],U,K,[B1|Bs1]) :-

	dim_atomname(B,U,K,B1),

	rename_atoms(Bs,U,K,Bs1).

rename_atoms([],_,_,[]).



dim_atomname(A,'()',K,(K,A1)) :-

	lowerbound(false),

	!,

	A =.. [P|Xs],

	atom_number(AK,K),

	atom_concat(AK,')',AK1),

	atom_concat('(',AK1,Suff),

	atom_concat(P,Suff,P1),

	A1 =.. [P1|Xs].

dim_atomname(A,'()',K,(K,A1)) :-

	lowerbound(true),

	!,

	A =.. [P|Xs],

	atom_number(AK,K),

	atom_concat(AK,'>',AK1),

	atom_concat('<',AK1,Suff),

	atom_concat(P,Suff,P1),

	A1 =.. [P1|Xs].

dim_atomname(A,'[]',K,(K,A1)) :-

	lowerbound(false),

	!,

	A =.. [P|Xs],

	atom_number(AK,K),

	atom_concat(AK,']',AK1),

	atom_concat('[',AK1,Suff),

	atom_concat(P,Suff,P1),

	A1 =.. [P1|Xs].

dim_atomname(A,'[]',K,(K,A1)) :-

	lowerbound(true),

	!,

	A =.. [P|Xs],

	atom_number(AK,K),

	atom_concat(AK,'}',AK1),

	atom_concat('{',AK1,Suff),

	atom_concat(P,Suff,P1),

	A1 =.. [P1|Xs].

	

separate_constraints([],[],[]).

separate_constraints([B|Bs],[(0,C)|Cs],Ds) :-

	constraint(B,C),

	!,

	separate_constraints(Bs,Cs,Ds).

separate_constraints([B|Bs],Cs,[B|Ds]) :-

	separate_constraints(Bs,Cs,Ds).



clauseIds(Ids) :-

	findall(C,my_clause(_,_,C),Ids).



% write out clauses



writeClauses([((_,H):-B1)|Rs],S) :-

	writeq(S,H),

	write(S,' :-'),

	sort(B1,B),

	nl(S),

	writeBodyAtoms(S,B),

	write(S,'.'),

	nl(S),

	writeClauses(Rs,S).

writeClauses([],_).

	

writeBodyAtoms(S,[]) :-

	!,

	write(S,'   '),

	write(S,true).

writeBodyAtoms(S,[(_,B)]) :-

	!,

	write(S,'   '),

	writeq(S,B).

writeBodyAtoms(S,[(_,B1),B2|Bs]) :-

	write(S,'   '),

	writeq(S,B1),

	write(S,','),

	nl(S),

	writeBodyAtoms(S,[B2|Bs]).



%---



constraint(X=Y, X=Y).

constraint(X=:=Y, X=Y).

constraint(X is Y, X = Y).

constraint(X>Y, X>Y).

constraint(X>=Y, X>=Y).

constraint(X=<Y, X=<Y).

constraint(X<Y, X<Y).



constraint(_\==_,0=0).

constraint(_=\=_,0=0).

constraint(true,0=0).

constraint(fail,1=0).



% --- load clauses file ----





load_file(F) :-

    retractall(my_clause(_,_,_)),

	open(F,read,S),

	remember_all(S,1),

	close(S).



remember_all(S,K) :-

	read(S,C),

	(

	    C == end_of_file -> true

	;

	    remember_clause(C,K),

	    K1 is K+1,

	    remember_all(S,K1)

	).



remember_clause((A :- B),K) :-

	!,

	tuple2list(B,BL),

	makeClauseId(K,CK),

	assert(my_clause(A,BL,CK)).



remember_clause(A,K) :-

	makeClauseId(K,CK),

	assert(my_clause(A,[],CK)),

	!.

remember_clause((:- _),_).



makeClauseId(K,CK) :-

	name(K,NK),

	name(c,C),

	append(C,NK,CNK),

	name(CK,CNK).



tuple2list((A,As),[A|LAs]) :-

	!,

	tuple2list(As,LAs).

tuple2list(A,[A]).

