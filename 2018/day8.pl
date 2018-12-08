#!/usr/bin/env swipl -O -t main --quiet

:- use_module(library(dcg/basics), [integer//1]).

node(node([], Ms)) --> integer(0), ` `, integer(M), ` `, meta(M, Ms).
node(node(Cs, Ms)) --> integer(C), ` `, integer(M), ` `, nodes(C, Cs), ` `, meta(M, Ms).

nodes(0, []) --> [].
nodes(1, [N]) --> node(N).
nodes(X, [N | Ns]) --> node(N), ` `, {XX is X - 1}, nodes(XX, Ns).

meta(0, []) --> [].
meta(1, [M]) --> integer(M).
meta(X, [M | Ms]) --> integer(M), {XX is X - 1}, ` `, meta(XX, Ms).

sum_metadata(node(Children, Meta), S) :-
  maplist(sum_metadata, Children, CS), sum_list(CS, S1), sum_list(Meta, S2), S is S1 + S2.

main :-
  read_string(user_input, _, S), string_codes(S, C), phrase(node(N), C, _),
  sum_metadata(N, Part1),
  writeln(Part1).
