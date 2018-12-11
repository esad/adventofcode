#!/usr/bin/env swipl -O -t main --quiet

:- use_module(library(rbtrees)).

start(z([],0,[])).

% Just a simple circular zipper:
z_next(z([], E, []), z([], E, [])).
z_next(z(B, E, [N|NN]), z([E|B], N, NN)).
z_next(z([B], E, []), z([E], B, [])).
z_next(z(B, E, []), z([], R, RR)) :- B = [_,_|_], reverse([E|B], [R|RR]).

z_insert(X, z(B, E, N), z([E|B], X, N)).
z_prev(Z1, Z2) :- z_next(Z2, Z1).
z_drop(z(B, E, [N|NN]), E, z(B, N, NN)).

insert(Z, N, Z3) :- z_next(Z, Z2), z_insert(N, Z2, Z3).
remove(Z, D, Zout) :-
  z_prev(Z, Z1), z_prev(Z1, Z2), z_prev(Z2, Z3), z_prev(Z3, Z4), z_prev(Z4, Z5), z_prev(Z5, Z6), z_prev(Z6,Z7),
  z_drop(Z7, D, Zout).

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
  turn(Player, Marble, S, T), !,
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
  start(DL),
  %rb_empty(X), play(0, 1, 9, 25, DL-X, S), _-H = S, highscore(H, _-P),
  rb_empty(X), play(0, 1, 463, 7178700, DL-X, S), _-H = S, highscore(H, _-P),
  writeln(P).
