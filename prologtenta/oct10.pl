% TASK 2

% X=f(Y,Z),
% Y=g(Z,a),
% Z=f(a,b).

% X = f(g(f(a,b),a),b).
% Y = g(f(a,b),a).
% Z = f(a,b).

% TASK 3
% gh(H,B) --> "f(", g(H), ",", h(B), ")".

% TASK 4
% get(t(t([1],[2]), t([3],[4])), X).
gett([X|Rest], [X|Rest]).
gett(t(A,B), Out) :-
    gett(A, OutA),
    gett(B, OutB),
    append(OutA, OutB, Out).

% TASK 5

getpos(0, [X|Xs], X).
getpos(P, [_|Xs], Out) :-
    NewP is P - 1,
    getpos(NewP, Xs, Out).

matrixelement(m(Matrix), X, Y, Element) :-
    getpos(Y, Matrix, n(Row)),
    getPos(X, Row, Element).

nonzero(Matrix, List) :-
    ok.

% TASK 6
/*
if declares a herbrand universe. All terms included and possibly yielded when
    computing if.

    herbrand universe:
        tr(a,b)
        tr(b,c),
        tr(c,a)
        true
        and(any_of_above, any_of_above)
*/

% TASK 7

