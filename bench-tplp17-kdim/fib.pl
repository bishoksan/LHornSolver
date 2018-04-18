
fib(X, Y):-  X>=0, X=<1, X=Y.
fib(X, Y) :-
        X > 1,
        X2 = X - 2, fib(X2, Y2),
        X1 = X - 1, fib(X1, Y1),
        Y = Y1 + Y2.
false:-
X>5, fib(X,Y), Y<X.

