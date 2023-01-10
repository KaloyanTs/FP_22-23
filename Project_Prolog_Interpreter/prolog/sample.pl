edge(a,b).
edge(b,c).
edge(a,d).
edge(d,c).
edge(X,Y) :- edge(Y,X).

self(X) :- edge(X,X).


