applen(X,Y,Z) :- X=0, Y=Z, Y>=0.
applen(X,Y,Z) :- applen(X1,Y,Z1), X = X1+1, Z = Z1+1.

revlen(X,Y) :- X=0,Y=0.
revlen(X,Y) :- revlen(X1,Z),applen(Z,U,Y),X=X1+1, U=1.
false :- revlen(X,Y), X>Y.
false :- revlen(X,Y), X<Y.
