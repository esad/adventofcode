#!/usr/bin/env swipl -O -t main --quiet

:- consult(shared).

:- use_module(library(dcg/basics), [integer//1]).
:- use_module(library(rbtrees)).

timestamp(ts(Y,MM,D,H,M)) --> `[`, integer(Y), `-`, integer(MM), `-`, integer(D), ` `, integer(H), `:`, integer(M), `] `.
line(log(Ts, asleep)) --> timestamp(Ts), `falls asleep`.
line(log(Ts, awake)) --> timestamp(Ts), `wakes up`.
line(log(Ts, start(G))) --> timestamp(Ts), `Guard #`, integer(G), ` begins shift`.
parse(Line, L) :- string_codes(Line, C), phrase(line(L), C).

sleeps([S]) --> sleep(S).
sleeps([S | Ss]) --> sleep(S), sleeps(Ss).
sleep(G-Ds) --> [log(_, start(G))], durations(Ds).
durations([D]) --> duration(D).
durations([D | Ds]) --> duration(D), durations(Ds).
durations([]) --> [].
duration(M1-M2) --> [log(ts(_,_,_,_,M1), asleep), log(ts(_,_,_,_,M2), awake)].

count([], 0).
count([M1-M2 | Ds], C2) :- count(Ds, C1), D is M2 - M1, C2 is C1 + D.

fill(G-Ds, Tree, Tree2) :-
  count(Ds, C),
  (rb_lookup(G, Total-OldDs, Tree)
  -> NewTotal is Total + C, rb_update(Tree, G, NewTotal-[Ds | OldDs], Tree2)
  ; rb_insert_new(Tree, G, C-Ds, Tree2)).

sleepiest(Entry, Max, Best) :- Entry = _-(Total-_), Max = _-(TotalMax-_), (Total > TotalMax -> Best = Entry ; Best = Max).

minute(M) :- between(0, 59, M).

sleeps_minute([], _, 0).
sleeps_minute([M1-M2|Ds], Min, Count) :-
  sleeps_minute(Ds, Min, C), M2S is M2 - 1, (between(M1, M2S, Min) -> Count is C + 1 ; Count = C).

guard_intervals(G-(_-Dss), guard_sleeps(G, Ds)) :- flatten(Dss, Ds).

main :-
  input(Lines),
  maplist(parse, Lines, Entries),
  sort(Entries, SortedEntries),
  phrase(sleeps(S), SortedEntries),
  rb_empty(T),
  foldl(fill, S, T, T2),
  rb_fold(sleepiest, T2, none-(0-[]), Guard-(Total-Dss)),
  flatten(Dss, Ds),
  aggregate(max(X,M), M, (minute(M), sleeps_minute(Ds, M, X)), max(_, Minute)),
  Result is Guard * Minute,
  writeln(Result),
  rb_visit(T2, Pairs),
  maplist(guard_intervals, Pairs, GI), assert_all(GI),
  aggregate_all(max(X,G-M), (guard_sleeps(G, GDs), minute(M), sleeps_minute(GDs, M, X)), max(_, Guard2-Minute2)),
  Result2 is Guard2 * Minute2,
  writeln(Result2).
