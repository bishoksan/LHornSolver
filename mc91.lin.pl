/* entry(go__0)*/

/*  go(1) :- go__0. */
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

/*  solve([false],1,1) :- solve__1. */
solve__1 :-
        A=<100,
        B>91,
        solve__2(A,B).
solve__1 :-
        A=<100,
        B=<90,
        solve__2(A,B).

/*  solve([mc91(A,B)],1,1) :- solve__2(A,B). */
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

/*  solve([],1,0) :- solve__3. */
solve__3.
/* Specialisation time 1.045999999999992 ms (runtime) */
