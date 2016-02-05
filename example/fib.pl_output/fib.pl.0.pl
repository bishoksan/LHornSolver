'fib(0)'(A,B) :-
   A>=0,
   A=<1,
   B=1.
'false(0)' :-
   A>5,
   B<A,
   'fib(0)'(A,B).
'false[0]' :-
   'false(0)'.
'fib[0]'(A,B) :-
   'fib(0)'(A,B).
