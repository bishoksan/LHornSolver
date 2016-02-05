'r7(0)' :-
   D=F+E,
   'r6(0)'(A,B,C,D,E,F).
'false(0)' :-
   D>F+E,
   'r6(0)'(A,B,C,D,E,F).
'false(0)' :-
   D<F+E,
   'r6(0)'(A,B,C,D,E,F).
'r5(0)'(A,B,C,D,E,F) :-
   F>=0,
   E>=0,
   'r4(0)'(A,B,C,G,H,I).
'r4(0)'(A,B,C,D,E,F) :-
   A=D,
   B=E,
   C=F.
'r3(0)'(A,B,C,D,E,F) :-
   D=I,
   H=F,
   I=E,
   'r2(0)'(A,C,B,G,H,I).
'r2(0)'(A,B,C,D,E,F) :-
   H=0,
   G=D,
   H=E,
   I=F,
   'r0(0)'(A,B,C,G,H,I).
'r1(0)'(A,B,C,D,E,F) :-
   H>0,
   G=D,
   H=E,
   I=F,
   'r0(0)'(A,B,C,G,H,I).
'r1(0)'(A,B,C,D,E,F) :-
   H<0,
   G=D,
   H=E,
   I=F,
   'r0(0)'(A,B,C,G,H,I).
'r0(0)'(A,B,C,D,E,F) :-
   C=L+1,
   B=K-1,
   A=D,
   C=F,
   B=E,
   'r1(0)'(G,H,I,J,K,L).
'r0(0)'(A,B,C,D,E,F) :-
   C=L,
   B=K,
   A=D,
   C=F,
   B=E,
   'r5(0)'(G,H,I,J,K,L).
'false[0]' :-
   'false(0)'.
'r0[0]'(A,B,C,D,E,F) :-
   'r0(0)'(A,B,C,D,E,F).
'r1[0]'(A,B,C,D,E,F) :-
   'r1(0)'(A,B,C,D,E,F).
'r2[0]'(A,B,C,D,E,F) :-
   'r2(0)'(A,B,C,D,E,F).
'r3[0]'(A,B,C,D,E,F) :-
   'r3(0)'(A,B,C,D,E,F).
'r4[0]'(A,B,C,D,E,F) :-
   'r4(0)'(A,B,C,D,E,F).
'r5[0]'(A,B,C,D,E,F) :-
   'r5(0)'(A,B,C,D,E,F).
'r6[0]'(A,B,C,D,E,F) :-
   'r6(0)'(A,B,C,D,E,F).
'r7[0]' :-
   'r7(0)'.
