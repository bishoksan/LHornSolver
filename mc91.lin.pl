/* entry(go__0)*/

/*  go(4) :- go__0. */
go__0 :-
        solve__1.
go__0 :-
        solve__1.
go__0 :-
        solve__1.
go__0 :-
        solve__1.
go__0 :-
        solve__1.
go__0 :-
        solve__1.

/*  solve([false],4,1) :- solve__1. */
solve__1 :-
        A=<100,
        B>91,
        solve__2(A,B).
solve__1 :-
        A=<100,
        B=<90,
        solve__2(A,B).

/*  solve([mc91(A,B)],4,1) :- solve__2(A,B). */
solve__2(A,B) :-
        A>100,
        B=A-10,
        solve__3.
solve__2(A,B) :-
        A>100,
        B=A-10,
        solve__3.
solve__2(A,B) :-
        A>100,
        B=A-10,
        solve__3.
solve__2(A,B) :-
        A=<100,
        C=A+11,
        solve__4(D,C,B,D).
solve__2(A,B) :-
        A=<100,
        C=A+11,
        solve__4(B,D,D,C).

/*  solve([],4,0) :- solve__3. */
solve__3.

/*  solve([mc91(A,B),mc91(C,D)],4,2) :- solve__4(B,A,D,C). */
solve__4(B,A,D,C) :-
        A>100,
        B=A-10,
        solve__2(C,D).
solve__4(B,A,D,C) :-
        A>100,
        B=A-10,
        solve__2(C,D).
solve__4(B,A,D,C) :-
        A>100,
        B=A-10,
        solve__2(C,D).
solve__4(B,A,D,C) :-
        A=<100,
        E=A+11,
        solve__5(C,D,F,B,E,F).
solve__4(B,A,D,C) :-
        A=<100,
        E=A+11,
        solve__5(C,D,E,F,F,B).

/*  solve([mc91(A,B),mc91(C,D),mc91(E,F)],4,3) :- solve__5(E,F,C,D,A,B). */
solve__5(E,F,C,D,A,B) :-
        A>100,
        B=A-10,
        solve__4(D,C,F,E).
solve__5(E,F,C,D,A,B) :-
        A>100,
        B=A-10,
        solve__4(D,C,F,E).
solve__5(E,F,C,D,A,B) :-
        A>100,
        B=A-10,
        solve__4(D,C,F,E).
solve__5(E,F,C,D,A,B) :-
        A=<100,
        G=A+11,
        solve__6(H,G,B,H,D,C,F,E).
solve__5(E,F,C,D,A,B) :-
        A=<100,
        G=A+11,
        solve__6(B,H,H,G,D,C,F,E).

/*  solve([mc91(A,B),mc91(C,D),mc91(E,F),mc91(G,H)],4,4) :- solve__6(B,A,D,C,F,E,H,G). */
solve__6(B,A,D,C,F,E,H,G) :-
        A>100,
        B=A-10,
        solve__5(G,H,E,F,C,D).
solve__6(B,A,D,C,F,E,H,G) :-
        A>100,
        B=A-10,
        solve__5(G,H,E,F,C,D).
solve__6(B,A,D,C,F,E,H,G) :-
        A>100,
        B=A-10,
        solve__5(G,H,E,F,C,D).
/* Specialisation time 2.461000000000013 ms (runtime) */
