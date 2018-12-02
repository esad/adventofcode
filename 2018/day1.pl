#!/usr/bin/env swipl -O -t main --quiet

input(R) :-
  read_string(user_input, _, S),
  split_string(S, "\n", "", L),
  exclude(=(""), L, R).

double_occurence(X, _, _, S, X) :- member(X, S).
double_occurence(X, A, [], S, Y) :- double_occurence(X, [], A, S, Y).
double_occurence(X, A, [C | Changes], Seen, Result) :-
  X2 is X + C,
  append(A, [C], A2),
  double_occurence(X2, A2, Changes, [X | Seen], Result).

main :-
  input(Lines),
  maplist(atom_number, Lines, Numbers),
  sumlist(Numbers, Part1),
  writeln(Part1),
  % Part 2 will take ~15 mins due to inefficient list lookups
  once(double_occurence(0, [], Numbers, [], Part2)),
  writeln(Part2).
