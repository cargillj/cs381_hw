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
schedule(X,P,T) :- enroll(X,C), where(C,P), when(C,T).
usage(P,T) :- where(C,P), when(C,T).  
conflict(X,Y) :- when(X,T), when(Y,T), where(X,P), where(Y,P), X =\= Y. 
