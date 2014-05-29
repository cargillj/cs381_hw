/* HW5: Prolog */
/* CS 381 Spring 2014 */
/* Authors: Aaron Egger, John Cargill */

/* Exercise 1. Database Application */
when(275,10).
when(261,12). 
when(381,11).
when(398,12).
when(399,12).
where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).
enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

/* (a) */
schedule(X,P,T) :- enroll(X,C), where(C,P), when(C,T).

/* (b) */
usage(P,T) :- where(C,P), when(C,T).

/* (c) */
conflict(X,Y) :- when(X,T), when(Y,T), where(X,P), where(Y,P), X =\= Y.

/* (d) */
meet(X,Y) :- enroll(X,C), enroll(Y,C), X =\= Y.
meet(X,Y) :- enroll(X,C), where(C,P), when(C,T), enroll(Y,D), where(D,P), when(D,T+1), X =\= Y.

/* Exercise 2. List Predicates and Arithmetic */

/* (a) */

/* (b) */

/* (c) */
