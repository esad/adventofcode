#!/usr/bin/env swipl -O -t main --quiet

input(R) :-
  read_string(user_input, _, S),
  split_string(S, "\n", "", L),
  exclude(=(""), L, R).

rle([], []).
rle([C], [[C]]).
rle([C | X], [[C] | Y]) :- X = [C2 | _], C \= C2, rle(X, Y).
rle([C | X], [[C | D] | Rest]) :- X = [C | _], rle(X, [D | Rest]).

process(S, R) :- string_chars(S, C), msort(C, L), rle(L, R).

double([[X, X] | _]).
double([_ | R]) :- double(R).

triple([[X, X, X] | _]).
triple([_ | R]) :- triple(R).

main :-
  input(Lines),
  maplist(process, Lines, Entries),
  include(double, Entries, Doubles), length(Doubles, D),
  include(triple, Entries, Triples), length(Triples, T),
  Result is D * T,
  writeln(Result).
