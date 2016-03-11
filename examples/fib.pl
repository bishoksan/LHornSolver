fib(A, B):- A>=0,  A=<1, B=1.
fib(A, B) :- A > 1, A2 = A - 2, fib(A2, B2),
    A1 = A - 1, fib(A1, B1), B = B1 + B2.
false:- A>5, fib(A,B), B<A.