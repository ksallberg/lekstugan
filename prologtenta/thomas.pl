:- [].

flatten(L,F) :-
    flattendl(L,F-[]).

atom_or_number(X) :-
    atomic(X),
    X \= [].

appenddl(A-B,B-C,A-C).

% Then you rewrite flattendl into a tail recursive version thus:
flattendl([], A-A).
flattendl([H|T], R) :-
    R1=A-B, R2=B-C, R=A-C,
    flattendl(H, R1),
    flattendl(T, R2).
flattendl(H, R) :-
    atom_or_number(H),
    R = [H|T]-T.

% This uses the difference list as an accumulator.
% If you really want to see that this is the case write like this:
% flatten2([[[1,2,3]]], X).
flatten2(L,F) :-
    flattenacc(L, F, []).

flattenacc([], A, A).
flattenacc([H|T], A, C) :-
    flattenacc(H, A, B),
    flattenacc(T, B, C).
flattenacc(H, [H|T], T) :-
    atom_or_number(H).

% flatten3([[[1,2,3]]], X).
flatten3([], []).
flatten3([A], NewA) :-
    flatten3(A, NewA).
flatten3([[A]|Rest], [NewA|Next]) :-
    flatten3(A, NewA),
    flatten3(Rest, Next).
flatten3([X|Rest], [NewX|Next]) :-
    flatten3(X, NewX),
    flatten3(Rest, Next).
flatten3(A,A).
