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
conflict(X,Y) :- when(X,T), when(Y,T), where(X,P), where(Y,P), X \= Y.

/* (d) */
meet(X,Y) :- schedule(X,P,T), schedule(Y,P,T), X \= Y;
             schedule(X,P,T1), schedule(Y,P,T), T1\==T2+1, X \= Y.

/* Exercise 2. List Predicates and Arithmetic */

/* (a) */
rdup(L,M) :- rdup2(L,M). 
rdup2([],[]). 
rdup2([HEAD|TAIL1], [HEAD|TAIL2]) :- rdup2(TAIL1,TAIL2),
				     not(member(HEAD,TAIL1)). 
rdup2([HEAD|TAIL1], TAIL2) :- rdup2(TAIL1,TAIL2), member(HEAD,TAIL1). 

/* (b) */
flat(L,F) :- flat(L, [], F). 
flat([], F, F). 
flat([HEAD|TAIL], L, F) :- flat(HEAD, L1, F), flat(TAIL, L, L1). 
flat(HEAD, L, [HEAD|L]) :- \+ is_list(HEAD). 

/* (c) */
element_at(X, List, Pos) :-  element_at(X, List, 1, Pos).
element_at(X, [X|_], Pos, Pos).
element_at(X, [_|T], Acc, Pos) :-  Acc1 is Acc + 1,
				   element_at(X, T, Acc1, Pos).

project(X, LIST, [HEAD|TAIL]) :- element_at(X, LIST, HEAD).
/*      		         append(X,Y,X),
			         project(X, LIST, TAIL). 
*/
