#!/usr/bin/env swipl -O -t main --quiet

:- consult(shared).

:- use_module(library(dcg/basics), [integer//1]).
:- use_module(library(rbtrees)).

rect(r(Id, X, Y, W, H)) --> `#`, integer(Id), ` @ `, integer(X), `,`, integer(Y), `: `, integer(W), `x`, integer(H).

parse(Line, R) :- string_codes(Line, C), phrase(rect(R), C).

point(r(_, X, Y, W, H), (Px, Py)) :- X2 is X+W-1, Y2 is Y+H-1, between(X, X2, Px), between(Y, Y2, Py).

fill(P, Tree, Tree2) :-
  rb_lookup(P, Count, Tree)
  -> NewCount is Count + 1, rb_update(Tree, P, NewCount, Tree2)
  ; rb_insert_new(Tree, P, 0, Tree2).

paint(R, Tree, Tree2) :-
  findall((X,Y), point(R, (X,Y)), Points),
  foldl(fill, Points, Tree, Tree2).

count(_-V, C, C2) :- V >= 1 -> C2 is C + 1 ; C2 is C.

coll(r(_, X1, Y1, W1, H1), r(_, X2, Y2, W2, H2)) :- side_collision(X1-W1, X2-W2), side_collision(Y1-H1, Y2-H2).

side_collision(A1-A2, B1-B2) :- A1 > B1, side_collision(B1-B2, A1-A2).
side_collision(A1-A2, B1-_) :- A1 =< B1, A2P is A1+A2-1, between(A1, A2P, B1).

exclusive(Rs, E) :- member(E, Rs), foreach((member(R, Rs), R \= E), \+ coll(E, R)).

main :-
  input(Lines),
  maplist(parse, Lines, Rs),
  rb_empty(T),
  foldl(paint, Rs, T, T2),
  rb_fold(count, T2, 0, Total),
  exclusive(Rs, E),
  writeln(Total), writeln(E).
