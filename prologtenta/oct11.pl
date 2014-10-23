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

getfirst([], [], []).
getfirst([[H|T]|R], [T|S], [H|U]) :- getfirst(R, S, U).

transpose([], []).
transpose([[]|_], []).
transpose(M, [H|T]) :- getfirst(M, R, H),
                       transpose(R, T).
