'fib(0)'(A,B) :-
   A>=0,
   A=<1,
   B=1.
'false(1)' :-
   A>5,
   B<A,
   'fib(1)'(A,B).
'false(0)' :-
   A>5,
   B<A,
   'fib(0)'(A,B).
'fib(1)'(A,B) :-
   A>1,
   C=A-2,
   E=A-1,
   B=F+D,
   'fib(1)'(C,D),
   'fib[0]'(E,F).
'fib(1)'(A,B) :-
   A>1,
   C=A-2,
   E=A-1,
   B=F+D,
   'fib[0]'(C,D),
   'fib(1)'(E,F).
'fib(1)'(A,B) :-
   A>1,
   C=A-2,
   E=A-1,
   B=F+D,
   'fib(0)'(C,D),
   'fib(0)'(E,F).
'false[1]' :-
   'false(1)'.
'false[1]' :-
   'false(0)'.
'false[0]' :-
   'false(0)'.
'fib[1]'(A,B) :-
   'fib(1)'(A,B).
'fib[1]'(A,B) :-
   'fib(0)'(A,B).
'fib[0]'(A,B) :-
   'fib(0)'(A,B).