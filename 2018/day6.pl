#!/usr/bin/env swipl -O -t main --quiet

:- consult(shared).

:- use_module(library(dcg/basics), [integer//1]).
:- use_module(library(aggregate)).

point(p(_, X, Y)) --> integer(X), `, `, integer(Y), `\n`. 
points([P | [PP | Ps]], W, H) --> 
  point(P), points([PP | Ps], W2, H2),
  { PP = p(II, _, _), I is II + 1, P = p(I, X, Y),
    W is max(X, W2), H is max(Y, H2)
  }.
points([P], X, Y) --> {P = p(0,X,Y)}, point(P).
points([], 0, 0) --> [].

on_canvas(X, Y) :- size(W, H), between(0, W, X), between(0, H, Y).

distance(D, I, X1, Y1) :- p(I, X, Y), D is abs(X1 - X) + abs(Y1 - Y).

color(I, X, Y) :- distance(D, I, X, Y), \+((dif(I, I3), distance(D2, I3, X, Y), D2 =< D)).

area(I, A) :- aggregate_all(count, (on_canvas(X,Y), color(I, X, Y)), A).

distance_sum(S, X, Y) :- aggregate_all(sum(D), (p(I, _, _), distance(D, I, X, Y)), S).

inf(I) :- on_canvas(0, Y), color(I, 0, Y).
inf(I) :- on_canvas(X, 0), color(I, X, 0).
inf(I) :- on_canvas(W, Y), size(W, _), color(I, W, Y).
inf(I) :- on_canvas(X, H), size(_, H), color(I, X, H).

main :-
  read_string(user_input, _, S), string_codes(S, C), phrase(points(Ps, W, H), C),
  assert_all(Ps),
  assert(size(W, H)),
  aggregate_all(max(A,I), (p(I,_,_), \+ inf(I), area(I, A)), max(P1,_)),
  aggregate_all(count, (on_canvas(X,Y), distance_sum(Sum, X, Y), Sum < 10000), P2),
  writeln(P1),
  writeln(P2).
