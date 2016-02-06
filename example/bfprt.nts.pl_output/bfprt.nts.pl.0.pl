'r12(0)' :-
   C<0,
   'r11(0)'(A,B,C,D).
'r12(0)' :-
   C>0,
   'r11(0)'(A,B,C,D).
'false(0)' :-
   C=0,
   'r11(0)'(A,B,C,D).
'r10(0)'(A,B,C,D) :-
   D>0,
   'r9(0)'(A,B,E,F).
'r9(0)'(A,B,C,D) :-
   A=C,
   B=D.
'r8(0)'(A,B,C,D) :-
   C=1,
   H=D,
   I=K,
   J=L,
   'r2(0)'(A,B,E,F,G,H,I,J).
'r8(0)'(A,B,C,D) :-
   C=G+I,
   H=D,
   I=K,
   J=L,
   'r7(0)'(A,B,E,F,G,H,I,J).
'r6(0)'(A,B,C,D,E,F,G,H) :-
   H>0,
   H<J,
   I=E,
   J=F,
   K=G,
   'r5(0)'(A,B,C,D,I,J,K,L).
'r5(0)'(A,B,C,D,E,F,G,H) :-
   E=I+K,
   J=F,
   K=G,
   L=H,
   'r4(0)'(A,B,C,D,I,J,K,L).
'r3(0)'(A,B,C,D,E,F,G,H) :-
   E=0,
   J=F,
   K=G,
   L=H,
   'r1(0)'(A,B,C,D,I,J,K,L).
'r2(0)'(A,B,C,D,E,F,G,H) :-
   J=<5,
   I=E,
   J=F,
   K=G,
   L=H,
   'r0(0)'(A,B,C,D,I,J,K,L).
'r1(0)'(A,B,C,D,E,F,G,H) :-
   J>5,
   I=E,
   J=F,
   K=G,
   L=H,
   'r0(0)'(A,B,C,D,I,J,K,L).
'r0(0)'(A,B,C,D,E,F,G,H) :-
   10*B=N,
   A=E,
   B=F,
   'r3(0)'(I,J,K,L,M,N,O,P).
'r0(0)'(A,B,C,D,E,F,G,H) :-
   B=P,
   A=E,
   B=F,
   'r6(0)'(I,J,K,L,M,N,O,P).
'r0(0)'(A,B,C,D,E,F,G,H) :-
   B=L,
   A=E,
   B=F,
   'r10(0)'(I,J,K,L).
'false[0]' :-
   'false(0)'.
'r0[0]'(A,B,C,D,E,F,G,H) :-
   'r0(0)'(A,B,C,D,E,F,G,H).
'r1[0]'(A,B,C,D,E,F,G,H) :-
   'r1(0)'(A,B,C,D,E,F,G,H).
'r10[0]'(A,B,C,D) :-
   'r10(0)'(A,B,C,D).
'r11[0]'(A,B,C,D) :-
   'r11(0)'(A,B,C,D).
'r12[0]' :-
   'r12(0)'.
'r2[0]'(A,B,C,D,E,F,G,H) :-
   'r2(0)'(A,B,C,D,E,F,G,H).
'r3[0]'(A,B,C,D,E,F,G,H) :-
   'r3(0)'(A,B,C,D,E,F,G,H).
'r4[0]'(A,B,C,D,E,F,G,H) :-
   'r4(0)'(A,B,C,D,E,F,G,H).
'r5[0]'(A,B,C,D,E,F,G,H) :-
   'r5(0)'(A,B,C,D,E,F,G,H).
'r6[0]'(A,B,C,D,E,F,G,H) :-
   'r6(0)'(A,B,C,D,E,F,G,H).
'r7[0]'(A,B,C,D,E,F,G,H) :-
   'r7(0)'(A,B,C,D,E,F,G,H).
'r8[0]'(A,B,C,D) :-
   'r8(0)'(A,B,C,D).
'r9[0]'(A,B,C,D) :-
   'r9(0)'(A,B,C,D).
