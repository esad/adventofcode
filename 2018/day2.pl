#!/usr/bin/env swipl -O -t main --quiet

:- consult(shared).

rle([], []).
rle([C], [[C]]).
rle([C | X], [[C] | Y]) :- X = [C2 | _], C \= C2, rle(X, Y).
rle([C | X], [[C | D] | Rest]) :- X = [C | _], rle(X, [D | Rest]).

process(S, R) :- string_chars(S, C), msort(C, L), rle(L, R).

double([[X, X] | _]).
double([_ | R]) :- double(R).

triple([[X, X, X] | _]).
triple([_ | R]) :- triple(R).

pair([X|Xs], (X, Y)) :- member(Y, Xs).
pair([_|Xs], P) :- pair(Xs, P).
pair([], _) :- false.

similar(([X|Xs], [Y|Xs])) :- dif(X, Y).
similar(([X|Xs], [X|Ys])) :- similar((Xs, Ys)).

main :-
  input(Lines),
  maplist(process, Lines, Entries),
  include(double, Entries, Doubles), length(Doubles, D),
  include(triple, Entries, Triples), length(Triples, T),
  Result is D * T,
  writeln(Result),
  maplist(string_chars, Lines, Words),
  findall(P, pair(Words, P), Pairs),
  include(similar, Pairs, MatchingPairs),
  writeln(MatchingPairs).
