go(3) :- go__0.
solve([false],3,1) :- solve__1.
solve([mc91(A,B)],3,1) :- solve__2(A,B).
solve([],3,0) :- solve__3.
solve([mc91(A,B),mc91(C,D)],3,2) :- solve__4(B,A,D,C).
solve([mc91(A,B),mc91(C,D),mc91(E,F)],3,3) :- solve__5(E,F,C,D,A,B).
