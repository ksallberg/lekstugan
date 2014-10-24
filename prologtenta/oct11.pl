:- [].

% TASK 2
/*
    X=f(Y,Z),
    X=f(Z,a).

    Y=Z=a, Y is unified to Z, which is in turn unified to a.
           By transitivity, Y is unified to a.
*/

% membertree(t(1,2), t(t(1,2), t(3,4))).
% membertree(t(3,4), t(t(1,2), t(3,4))).

% membertree(t(1,2), t(t(1,2), t(3, t(4, nil)))).
% membertree(X, t(t(1,2), t(3, t(4, nil)))).

% TASK 3
membertree(X, t(X, _)).
membertree(X, t(Y, _)) :-
    membertree(X, Y).
membertree(X, t(_, X)).
membertree(X, t(_, Y)) :-
    membertree(X, Y).

% TASK 4
% traversing left to right
% elements(t(t(1,2), t(3, t(4, nil))), X).
elements(Tree, Ls) :-
    findall(Element, membertree(Element, Tree), Ls).

% transpose([[1,2],[3,4]],X).
% TASK 5
/*
transpose(Matrix, [Line|NewLine]) :-
    take_round(Matrix, Line),
    write(Line),
    (Line == [] ->
        NewLine = []
    ;
        tailm(Matrix, NewMatrix),
        transpose(NewMatrix, NewLine)
    ).

tailm([], []).
tailm([Line|Lines], [TailLine|Next]) :-
    tail(Line, TailLine),
    tailm(Lines, Next).

tail([_|T], T).

take_round([H], [H]) :- !.
take_round([[H|_]|Xs], [H|Next]) :-
    take_round(Xs, Next).
*/

/*
getfirst([], [], []).
getfirst([[H|T]|R], [T|S], [H|U]) :- getfirst(R, S, U).

transpose([], []).
transpose([[]|_], []).
transpose(M, [H|T]) :- getfirst(M, R, H),
                       transpose(R, T).
*/

% transpose([[1,2],[3,4]],X).

headall([], [], []).
headall([[H|T]|R], Tails, Heads) :-
    Tails = [T|NextTails],
    Heads = [H|NextHeads],
    headall(R, NextTails, NextHeads).

transpose([[]|_], []).
transpose(Matrix, [Heads|NextHeads]) :-
    headall(Matrix, Rest, Heads),
    transpose(Rest, NextHeads).

% TASK 7
test([1,2,3,4], 4, [6,5], 2).
test2([1,2], 2, [6,5,4,3], 4).

addleftseq(Element, seq(InList, InListLen, X, Y), S) :-
    NewLen is InListLen + 1,
    S = seq([Element|InList], NewLen, X, Y).

addrightseq(Element, seq(X, Y, InList, InListLen), S) :-
    NewLen is InListLen + 1,
    S = seq(X, Y, [Element|InList], NewLen).

balanceseq(seq(S1, L1, S2, L2), seq(B1, Lb1, B2, Lb2)) :-
    reverse(S2, R2),
    append(S1, R2, List),
    ListLen is L1 + L2,
    Lb1 is ListLen//2,
    length(B1, Lb1),
    append(B1, Br2, List),
    rev(Br2, B2),
    Lb2 is ListLen-Lb1.
