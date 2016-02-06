'r12(2)' :-
   C<0,
   'r11(2)'(A,B,C,D).
'r12(1)' :-
   C<0,
   'r11(1)'(A,B,C,D).
'r12(0)' :-
   C<0,
   'r11(0)'(A,B,C,D).
'r12(2)' :-
   C>0,
   'r11(2)'(A,B,C,D).
'r12(1)' :-
   C>0,
   'r11(1)'(A,B,C,D).
'r12(0)' :-
   C>0,
   'r11(0)'(A,B,C,D).
'false(2)' :-
   C=0,
   'r11(2)'(A,B,C,D).
'false(1)' :-
   C=0,
   'r11(1)'(A,B,C,D).
'false(0)' :-
   C=0,
   'r11(0)'(A,B,C,D).
'r10(2)'(A,B,C,D) :-
   D>0,
   'r9(2)'(A,B,E,F).
'r10(1)'(A,B,C,D) :-
   D>0,
   'r9(1)'(A,B,E,F).
'r10(0)'(A,B,C,D) :-
   D>0,
   'r9(0)'(A,B,E,F).
'r9(0)'(A,B,C,D) :-
   A=C,
   B=D.
'r8(2)'(A,B,C,D) :-
   C=1,
   H=D,
   I=K,
   J=L,
   'r2(2)'(A,B,E,F,G,H,I,J).
'r8(1)'(A,B,C,D) :-
   C=1,
   H=D,
   I=K,
   J=L,
   'r2(1)'(A,B,E,F,G,H,I,J).
'r8(0)'(A,B,C,D) :-
   C=1,
   H=D,
   I=K,
   J=L,
   'r2(0)'(A,B,E,F,G,H,I,J).
'r8(2)'(A,B,C,D) :-
   C=G+I,
   H=D,
   I=K,
   J=L,
   'r7(2)'(A,B,E,F,G,H,I,J).
'r8(1)'(A,B,C,D) :-
   C=G+I,
   H=D,
   I=K,
   J=L,
   'r7(1)'(A,B,E,F,G,H,I,J).
'r8(0)'(A,B,C,D) :-
   C=G+I,
   H=D,
   I=K,
   J=L,
   'r7(0)'(A,B,E,F,G,H,I,J).
'r6(2)'(A,B,C,D,E,F,G,H) :-
   H>0,
   H<J,
   I=E,
   J=F,
   K=G,
   'r5(2)'(A,B,C,D,I,J,K,L).
'r6(1)'(A,B,C,D,E,F,G,H) :-
   H>0,
   H<J,
   I=E,
   J=F,
   K=G,
   'r5(1)'(A,B,C,D,I,J,K,L).
'r6(0)'(A,B,C,D,E,F,G,H) :-
   H>0,
   H<J,
   I=E,
   J=F,
   K=G,
   'r5(0)'(A,B,C,D,I,J,K,L).
'r5(2)'(A,B,C,D,E,F,G,H) :-
   E=I+K,
   J=F,
   K=G,
   L=H,
   'r4(2)'(A,B,C,D,I,J,K,L).
'r5(1)'(A,B,C,D,E,F,G,H) :-
   E=I+K,
   J=F,
   K=G,
   L=H,
   'r4(1)'(A,B,C,D,I,J,K,L).
'r5(0)'(A,B,C,D,E,F,G,H) :-
   E=I+K,
   J=F,
   K=G,
   L=H,
   'r4(0)'(A,B,C,D,I,J,K,L).
'r3(2)'(A,B,C,D,E,F,G,H) :-
   E=0,
   J=F,
   K=G,
   L=H,
   'r1(2)'(A,B,C,D,I,J,K,L).
'r3(1)'(A,B,C,D,E,F,G,H) :-
   E=0,
   J=F,
   K=G,
   L=H,
   'r1(1)'(A,B,C,D,I,J,K,L).
'r3(0)'(A,B,C,D,E,F,G,H) :-
   E=0,
   J=F,
   K=G,
   L=H,
   'r1(0)'(A,B,C,D,I,J,K,L).
'r2(2)'(A,B,C,D,E,F,G,H) :-
   J=<5,
   I=E,
   J=F,
   K=G,
   L=H,
   'r0(2)'(A,B,C,D,I,J,K,L).
'r2(1)'(A,B,C,D,E,F,G,H) :-
   J=<5,
   I=E,
   J=F,
   K=G,
   L=H,
   'r0(1)'(A,B,C,D,I,J,K,L).
'r2(0)'(A,B,C,D,E,F,G,H) :-
   J=<5,
   I=E,
   J=F,
   K=G,
   L=H,
   'r0(0)'(A,B,C,D,I,J,K,L).
'r1(2)'(A,B,C,D,E,F,G,H) :-
   J>5,
   I=E,
   J=F,
   K=G,
   L=H,
   'r0(2)'(A,B,C,D,I,J,K,L).
'r1(1)'(A,B,C,D,E,F,G,H) :-
   J>5,
   I=E,
   J=F,
   K=G,
   L=H,
   'r0(1)'(A,B,C,D,I,J,K,L).
'r1(0)'(A,B,C,D,E,F,G,H) :-
   J>5,
   I=E,
   J=F,
   K=G,
   L=H,
   'r0(0)'(A,B,C,D,I,J,K,L).
'r4(2)'(A,B,C,D,E,F,G,H) :-
   10*M=J,
   O=G,
   I=E,
   J=F,
   L=H,
   'r3(2)'(A,B,C,D,I,J,K,L),
   'r8[1]'(N,M,O,P).
'r4(2)'(A,B,C,D,E,F,G,H) :-
   10*M=J,
   O=G,
   I=E,
   J=F,
   L=H,
   'r3[1]'(A,B,C,D,I,J,K,L),
   'r8(2)'(N,M,O,P).
'r4(2)'(A,B,C,D,E,F,G,H) :-
   10*M=J,
   O=G,
   I=E,
   J=F,
   L=H,
   'r3(1)'(A,B,C,D,I,J,K,L),
   'r8(1)'(N,M,O,P).
'r4(1)'(A,B,C,D,E,F,G,H) :-
   10*M=J,
   O=G,
   I=E,
   J=F,
   L=H,
   'r3(1)'(A,B,C,D,I,J,K,L),
   'r8[0]'(N,M,O,P).
'r4(1)'(A,B,C,D,E,F,G,H) :-
   10*M=J,
   O=G,
   I=E,
   J=F,
   L=H,
   'r3[0]'(A,B,C,D,I,J,K,L),
   'r8(1)'(N,M,O,P).
'r4(1)'(A,B,C,D,E,F,G,H) :-
   10*M=J,
   O=G,
   I=E,
   J=F,
   L=H,
   'r3(0)'(A,B,C,D,I,J,K,L),
   'r8(0)'(N,M,O,P).
'r0(2)'(A,B,C,D,E,F,G,H) :-
   10*B=N,
   A=E,
   B=F,
   'r3(2)'(I,J,K,L,M,N,O,P).
'r0(1)'(A,B,C,D,E,F,G,H) :-
   10*B=N,
   A=E,
   B=F,
   'r3(1)'(I,J,K,L,M,N,O,P).
'r0(0)'(A,B,C,D,E,F,G,H) :-
   10*B=N,
   A=E,
   B=F,
   'r3(0)'(I,J,K,L,M,N,O,P).
'r7(2)'(A,B,C,D,E,F,G,H) :-
   M=L,
   O=G,
   I=E,
   J=F,
   L=H,
   'r6(2)'(A,B,C,D,I,J,K,L),
   'r8[1]'(N,M,O,P).
'r7(2)'(A,B,C,D,E,F,G,H) :-
   M=L,
   O=G,
   I=E,
   J=F,
   L=H,
   'r6[1]'(A,B,C,D,I,J,K,L),
   'r8(2)'(N,M,O,P).
'r7(2)'(A,B,C,D,E,F,G,H) :-
   M=L,
   O=G,
   I=E,
   J=F,
   L=H,
   'r6(1)'(A,B,C,D,I,J,K,L),
   'r8(1)'(N,M,O,P).
'r7(1)'(A,B,C,D,E,F,G,H) :-
   M=L,
   O=G,
   I=E,
   J=F,
   L=H,
   'r6(1)'(A,B,C,D,I,J,K,L),
   'r8[0]'(N,M,O,P).
'r7(1)'(A,B,C,D,E,F,G,H) :-
   M=L,
   O=G,
   I=E,
   J=F,
   L=H,
   'r6[0]'(A,B,C,D,I,J,K,L),
   'r8(1)'(N,M,O,P).
'r7(1)'(A,B,C,D,E,F,G,H) :-
   M=L,
   O=G,
   I=E,
   J=F,
   L=H,
   'r6(0)'(A,B,C,D,I,J,K,L),
   'r8(0)'(N,M,O,P).
'r0(2)'(A,B,C,D,E,F,G,H) :-
   B=P,
   A=E,
   B=F,
   'r6(2)'(I,J,K,L,M,N,O,P).
'r0(1)'(A,B,C,D,E,F,G,H) :-
   B=P,
   A=E,
   B=F,
   'r6(1)'(I,J,K,L,M,N,O,P).
'r0(0)'(A,B,C,D,E,F,G,H) :-
   B=P,
   A=E,
   B=F,
   'r6(0)'(I,J,K,L,M,N,O,P).
'r11(2)'(A,B,C,D) :-
   G=F,
   I=C,
   F=D,
   'r10(2)'(A,B,E,F),
   'r8[1]'(H,G,I,J).
'r11(2)'(A,B,C,D) :-
   G=F,
   I=C,
   F=D,
   'r10[1]'(A,B,E,F),
   'r8(2)'(H,G,I,J).
'r11(2)'(A,B,C,D) :-
   G=F,
   I=C,
   F=D,
   'r10(1)'(A,B,E,F),
   'r8(1)'(H,G,I,J).
'r11(1)'(A,B,C,D) :-
   G=F,
   I=C,
   F=D,
   'r10(1)'(A,B,E,F),
   'r8[0]'(H,G,I,J).
'r11(1)'(A,B,C,D) :-
   G=F,
   I=C,
   F=D,
   'r10[0]'(A,B,E,F),
   'r8(1)'(H,G,I,J).
'r11(1)'(A,B,C,D) :-
   G=F,
   I=C,
   F=D,
   'r10(0)'(A,B,E,F),
   'r8(0)'(H,G,I,J).
'r0(2)'(A,B,C,D,E,F,G,H) :-
   B=L,
   A=E,
   B=F,
   'r10(2)'(I,J,K,L).
'r0(1)'(A,B,C,D,E,F,G,H) :-
   B=L,
   A=E,
   B=F,
   'r10(1)'(I,J,K,L).
'r0(0)'(A,B,C,D,E,F,G,H) :-
   B=L,
   A=E,
   B=F,
   'r10(0)'(I,J,K,L).
'false[2]' :-
   'false(2)'.
'false[2]' :-
   'false(1)'.
'false[2]' :-
   'false(0)'.
'false[1]' :-
   'false(1)'.
'false[1]' :-
   'false(0)'.
'false[0]' :-
   'false(0)'.
'r0[2]'(A,B,C,D,E,F,G,H) :-
   'r0(2)'(A,B,C,D,E,F,G,H).
'r0[2]'(A,B,C,D,E,F,G,H) :-
   'r0(1)'(A,B,C,D,E,F,G,H).
'r0[2]'(A,B,C,D,E,F,G,H) :-
   'r0(0)'(A,B,C,D,E,F,G,H).
'r0[1]'(A,B,C,D,E,F,G,H) :-
   'r0(1)'(A,B,C,D,E,F,G,H).
'r0[1]'(A,B,C,D,E,F,G,H) :-
   'r0(0)'(A,B,C,D,E,F,G,H).
'r0[0]'(A,B,C,D,E,F,G,H) :-
   'r0(0)'(A,B,C,D,E,F,G,H).
'r1[2]'(A,B,C,D,E,F,G,H) :-
   'r1(2)'(A,B,C,D,E,F,G,H).
'r1[2]'(A,B,C,D,E,F,G,H) :-
   'r1(1)'(A,B,C,D,E,F,G,H).
'r1[2]'(A,B,C,D,E,F,G,H) :-
   'r1(0)'(A,B,C,D,E,F,G,H).
'r1[1]'(A,B,C,D,E,F,G,H) :-
   'r1(1)'(A,B,C,D,E,F,G,H).
'r1[1]'(A,B,C,D,E,F,G,H) :-
   'r1(0)'(A,B,C,D,E,F,G,H).
'r1[0]'(A,B,C,D,E,F,G,H) :-
   'r1(0)'(A,B,C,D,E,F,G,H).
'r10[2]'(A,B,C,D) :-
   'r10(2)'(A,B,C,D).
'r10[2]'(A,B,C,D) :-
   'r10(1)'(A,B,C,D).
'r10[2]'(A,B,C,D) :-
   'r10(0)'(A,B,C,D).
'r10[1]'(A,B,C,D) :-
   'r10(1)'(A,B,C,D).
'r10[1]'(A,B,C,D) :-
   'r10(0)'(A,B,C,D).
'r10[0]'(A,B,C,D) :-
   'r10(0)'(A,B,C,D).
'r11[2]'(A,B,C,D) :-
   'r11(2)'(A,B,C,D).
'r11[2]'(A,B,C,D) :-
   'r11(1)'(A,B,C,D).
'r11[2]'(A,B,C,D) :-
   'r11(0)'(A,B,C,D).
'r11[1]'(A,B,C,D) :-
   'r11(1)'(A,B,C,D).
'r11[1]'(A,B,C,D) :-
   'r11(0)'(A,B,C,D).
'r11[0]'(A,B,C,D) :-
   'r11(0)'(A,B,C,D).
'r12[2]' :-
   'r12(2)'.
'r12[2]' :-
   'r12(1)'.
'r12[2]' :-
   'r12(0)'.
'r12[1]' :-
   'r12(1)'.
'r12[1]' :-
   'r12(0)'.
'r12[0]' :-
   'r12(0)'.
'r2[2]'(A,B,C,D,E,F,G,H) :-
   'r2(2)'(A,B,C,D,E,F,G,H).
'r2[2]'(A,B,C,D,E,F,G,H) :-
   'r2(1)'(A,B,C,D,E,F,G,H).
'r2[2]'(A,B,C,D,E,F,G,H) :-
   'r2(0)'(A,B,C,D,E,F,G,H).
'r2[1]'(A,B,C,D,E,F,G,H) :-
   'r2(1)'(A,B,C,D,E,F,G,H).
'r2[1]'(A,B,C,D,E,F,G,H) :-
   'r2(0)'(A,B,C,D,E,F,G,H).
'r2[0]'(A,B,C,D,E,F,G,H) :-
   'r2(0)'(A,B,C,D,E,F,G,H).
'r3[2]'(A,B,C,D,E,F,G,H) :-
   'r3(2)'(A,B,C,D,E,F,G,H).
'r3[2]'(A,B,C,D,E,F,G,H) :-
   'r3(1)'(A,B,C,D,E,F,G,H).
'r3[2]'(A,B,C,D,E,F,G,H) :-
   'r3(0)'(A,B,C,D,E,F,G,H).
'r3[1]'(A,B,C,D,E,F,G,H) :-
   'r3(1)'(A,B,C,D,E,F,G,H).
'r3[1]'(A,B,C,D,E,F,G,H) :-
   'r3(0)'(A,B,C,D,E,F,G,H).
'r3[0]'(A,B,C,D,E,F,G,H) :-
   'r3(0)'(A,B,C,D,E,F,G,H).
'r4[2]'(A,B,C,D,E,F,G,H) :-
   'r4(2)'(A,B,C,D,E,F,G,H).
'r4[2]'(A,B,C,D,E,F,G,H) :-
   'r4(1)'(A,B,C,D,E,F,G,H).
'r4[2]'(A,B,C,D,E,F,G,H) :-
   'r4(0)'(A,B,C,D,E,F,G,H).
'r4[1]'(A,B,C,D,E,F,G,H) :-
   'r4(1)'(A,B,C,D,E,F,G,H).
'r4[1]'(A,B,C,D,E,F,G,H) :-
   'r4(0)'(A,B,C,D,E,F,G,H).
'r4[0]'(A,B,C,D,E,F,G,H) :-
   'r4(0)'(A,B,C,D,E,F,G,H).
'r5[2]'(A,B,C,D,E,F,G,H) :-
   'r5(2)'(A,B,C,D,E,F,G,H).
'r5[2]'(A,B,C,D,E,F,G,H) :-
   'r5(1)'(A,B,C,D,E,F,G,H).
'r5[2]'(A,B,C,D,E,F,G,H) :-
   'r5(0)'(A,B,C,D,E,F,G,H).
'r5[1]'(A,B,C,D,E,F,G,H) :-
   'r5(1)'(A,B,C,D,E,F,G,H).
'r5[1]'(A,B,C,D,E,F,G,H) :-
   'r5(0)'(A,B,C,D,E,F,G,H).
'r5[0]'(A,B,C,D,E,F,G,H) :-
   'r5(0)'(A,B,C,D,E,F,G,H).
'r6[2]'(A,B,C,D,E,F,G,H) :-
   'r6(2)'(A,B,C,D,E,F,G,H).
'r6[2]'(A,B,C,D,E,F,G,H) :-
   'r6(1)'(A,B,C,D,E,F,G,H).
'r6[2]'(A,B,C,D,E,F,G,H) :-
   'r6(0)'(A,B,C,D,E,F,G,H).
'r6[1]'(A,B,C,D,E,F,G,H) :-
   'r6(1)'(A,B,C,D,E,F,G,H).
'r6[1]'(A,B,C,D,E,F,G,H) :-
   'r6(0)'(A,B,C,D,E,F,G,H).
'r6[0]'(A,B,C,D,E,F,G,H) :-
   'r6(0)'(A,B,C,D,E,F,G,H).
'r7[2]'(A,B,C,D,E,F,G,H) :-
   'r7(2)'(A,B,C,D,E,F,G,H).
'r7[2]'(A,B,C,D,E,F,G,H) :-
   'r7(1)'(A,B,C,D,E,F,G,H).
'r7[2]'(A,B,C,D,E,F,G,H) :-
   'r7(0)'(A,B,C,D,E,F,G,H).
'r7[1]'(A,B,C,D,E,F,G,H) :-
   'r7(1)'(A,B,C,D,E,F,G,H).
'r7[1]'(A,B,C,D,E,F,G,H) :-
   'r7(0)'(A,B,C,D,E,F,G,H).
'r7[0]'(A,B,C,D,E,F,G,H) :-
   'r7(0)'(A,B,C,D,E,F,G,H).
'r8[2]'(A,B,C,D) :-
   'r8(2)'(A,B,C,D).
'r8[2]'(A,B,C,D) :-
   'r8(1)'(A,B,C,D).
'r8[2]'(A,B,C,D) :-
   'r8(0)'(A,B,C,D).
'r8[1]'(A,B,C,D) :-
   'r8(1)'(A,B,C,D).
'r8[1]'(A,B,C,D) :-
   'r8(0)'(A,B,C,D).
'r8[0]'(A,B,C,D) :-
   'r8(0)'(A,B,C,D).
'r9[2]'(A,B,C,D) :-
   'r9(2)'(A,B,C,D).
'r9[2]'(A,B,C,D) :-
   'r9(1)'(A,B,C,D).
'r9[2]'(A,B,C,D) :-
   'r9(0)'(A,B,C,D).
'r9[1]'(A,B,C,D) :-
   'r9(1)'(A,B,C,D).
'r9[1]'(A,B,C,D) :-
   'r9(0)'(A,B,C,D).
'r9[0]'(A,B,C,D) :-
   'r9(0)'(A,B,C,D).
