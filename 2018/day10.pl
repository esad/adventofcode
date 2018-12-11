#!/usr/bin/env swipl -O -t main --quiet

:- consult(shared).

:- use_module(library(dcg/basics), [integer//1, blanks//0]).

point([X,Y]-[Vx, Vy]) -->
  `position=<`, blanks, integer(X), `,`, blanks, integer(Y), `> `,
  `velocity=<`, blanks, integer(Vx), `,`, blanks, integer(Vy), `>`.

move([X,Y]-[Vx, Vy], [X2,Y2]-[Vx, Vy]) :- X2 is X + Vx, Y2 is Y + Vy.

deduplicate([], []).
deduplicate([R1,R2|T], L) :- R1=[X,Y]-_, R2=[X,Y]-_, !, deduplicate([R2|T], L).
deduplicate([H|T], L) :- deduplicate(T, L1), L=[H|L1].

interesting([[X, Y]-_, [X, Y1]-_, [X, Y2]-_, [X, Y3]-_, [X, Y4]-_, [X, Y5]-_|_]) :- 
  Y1 is Y + 1, Y2 is Y1 + 1, Y3 is Y2 + 1, Y4 is Y3 + 1, Y5 is Y4 + 1.

interesting([_|T]) :- interesting(T).

solution(Points, I, R) :-
  sort(Points, S), 
  ((deduplicate(S, D), interesting(D)) -> R = S-I ; maplist(move, S, S2), I1 is I + 1, solution(S2, I1, R)).

parse(Line, P) :- string_codes(Line, C), phrase(point(P), C, _).

dump([]).
dump([[Y,X]-_|T]) :-
  write(X), write(","), writeln(Y), dump(T).

main :-
  input(Lines),
  maplist(parse, Lines, Points),
  solution(Points, 0, P-I),
  writeln(I),
  dump(P).
