#!/usr/bin/env swipl -O -t main --quiet

:- consult(shared).

:- use_module(library(ctypes)).

reacts(C, C2) :- upper_lower(C, C2).
reacts(C, C2) :- upper_lower(C2, C).

step([C | Xs], X, Ignore, Y) :- member(C, Ignore), step(Xs, X, Ignore, Y).
step([], [], _, false).
step([C], [C], _, false).
step([C | [C2 | Cs]], X, I, P) :- reacts(C, C2) -> (step(Cs, X, I, _), P = true) ; (step([C2 | Cs], Y, I, P), X = [C | Y]).

process(S, Sout, Ignore) :- step(S, S2, Ignore, true), !, process(S2, Sout, Ignore).
process(S, S2, Ignore) :- step(S, S2, Ignore, false).

types([X, L]) :- is_alpha(X), is_upper(X), upper_lower(X, L).

main :-
  input([Line]),
  string_codes(Line, C),
  process(C, C2, []), 
  length(C2, L),
  writeln(L),
  aggregate_all(min(L2, T), (types(T), process(C, CI, T), length(CI, L2), writeln(T-L2)), Result),
  writeln(Result).
