:- [].

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


