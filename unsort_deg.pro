%% unsort degree
unsort_deg(Xs, D) :-
    unsort_deg_help(Xs, 0, D).

unsort_deg_help([], Final, Final).
unsort_deg_help([H|T], Prev, Next) :-
    degree(H, T, 0, HeadDegree),
    This is Prev + HeadDegree,
    unsort_deg_help(T, This, Next).

%% how many of the Ts is H larger than?
degree(Num, [], Final, Final).
degree(Num, [H|T], Prev, Next) :-
    Num > H,
    This is Prev + 1,
    degree(Num, T, This, Next).
degree(Num, [H|T], Prev, Next) :-
    degree(Num, T, Prev, Next).
