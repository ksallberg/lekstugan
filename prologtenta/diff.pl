:- [].

reverse(X, Y) :- reverse_dl(X, Y - []).

reverse_dl([], Xs-Xs).
reverse_dl([X|Xs], Ys-Zs) :-
    reverse_dl(Xs, Ys-[X|Zs]).

append_own(A-B, B-C, A-C).

test(A-B, B).
