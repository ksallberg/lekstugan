% familjetrad i prolog
female(anna).
female(maria).
male(ceasar).
male(rolf).
male(demonilla).
male(dracula).

mother(maria,anna).
mother(anna,rolf).
father(ceasar,rolf).
father(demonilla,maria).
father(demonilla,dracula).
father(dracula,rolf).

parent(X,Y) :- mother(X,Y);
               father(X,Y).

daugther(X) :- female(X),
               parent(Y,X).

son(Parent,X) :- male(X),
                 parent(Parent,X).

grand_son(GrandPar,X) :- male(X),
                         parent(Parent,X),
                         parent(GrandPar,Parent).

grand_par(GP) :- parent(GP,X),
                 parent(X,Y).

all_male([]).
all_male([X|Xs]) :- male(X),
                    all_male(Xs).

% the resulting list is seen as Y in the first pattern
reverse([],X).
reverse([X|Xs],Sum) :- reverse(Xs,NextPart),
                       append(NextPart,[X],Sum).

% viktigt att Y är under suml
suml([],0).
suml([X|Xs],Y) :- suml(Xs,Next),
                  Y is Next+X.

filter([],[]).
filter([X|Xs],Filtered) :-
   filter(Xs,Next),
   (mod(X,2) =:= 0 ->
      append([X], Next, Filtered);
      Filtered = Next).
