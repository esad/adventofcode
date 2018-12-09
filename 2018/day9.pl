#!/usr/bin/env swipl -O -t main --quiet

:- use_module(library(dialect/hprolog), [split_at/4]).
:- use_module(library(rbtrees)).

insert([X], N, [N, X]).
insert([X | [Y | Rest]], N, Result) :- append([N | Rest], [X, Y], Result).

remove(Circle, Dropped, Circle2) :-
  length(Circle, L),
  C is L - 7,
  split_at(C, Circle, X, Y),
  [Dropped | YY] = Y,
  append(YY, X, Circle2).

add_score(Scoreboard, Player, Points, Scoreboard2) :-
  rb_lookup(Player, Score, Scoreboard)
  -> NewScore is Score + Points, rb_update(Scoreboard, Player, NewScore, Scoreboard2)
  ; rb_insert_new(Scoreboard, Player, Points, Scoreboard2).

turn(Player, Marble, Circle-Scoreboard, Circle2-Scoreboard2) :-
  (Marble mod 23) =:= 0 ->
    remove(Circle, Dropped, Circle2),
    Points is Dropped + Marble,
    add_score(Scoreboard, Player, Points, Scoreboard2)
  ;
    insert(Circle, Marble, Circle2), Scoreboard2 = Scoreboard.

play(Player, Marble, Players, Marbles, S, S2) :-
  turn(Player, Marble, S, T),
  (Marble < Marbles ->
    P is (Player + 1) mod Players,
    M is Marble + 1,
    play(P, M, Players, Marbles, T, S2)
  ;
    S2 = T
  ).

best(Player-Score, Best-Highscore, Result) :- Score > Highscore -> Result = Player-Score ; Result = Best-Highscore.
highscore(Scoreboard, S) :- rb_fold(best, Scoreboard, none-0, S).

main :- 
  rb_empty(X), play(0, 1, 463, 71787, [0]-X, S), _-H = S, highscore(H, _-P),
  writeln(P).
