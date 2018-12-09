#!/usr/bin/env swipl -O -t main --quiet

:- use_module(library(rbtrees)).

insert([X,Y|T]-[X,Y|L], N, [N|T]-L).

start([0|T]-T).

append_dl(A-B, B-C, A-C).

remove([D,A1,A2,A3,A4,A5,A6]-[], D, [A1,A2,A3,A4,A5,A6|L]-L, X-X) :- !.
remove([H|Rest]-T, D, P-PP, [H|R]-RR) :- remove(Rest-T, D, P-PP, R-RR). 

add_score(Scoreboard, Player, Points, Scoreboard2) :-
  rb_lookup(Player, Score, Scoreboard)
  -> NewScore is Score + Points, rb_update(Scoreboard, Player, NewScore, Scoreboard2)
  ; rb_insert_new(Scoreboard, Player, Points, Scoreboard2).

turn(Player, Marble, Circle-Scoreboard, Circle2-Scoreboard2) :-
  writeln(Marble),
  (Marble mod 23) =:= 0 ->
    remove(Circle, Dropped, P1, P2),
    append_dl(P1, P2, Circle2),
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
    S2 = _-[]-_,
    S2 = T
  ).

best(Player-Score, Best-Highscore, Result) :- Score > Highscore -> Result = Player-Score ; Result = Best-Highscore.
highscore(Scoreboard, S) :- rb_fold(best, Scoreboard, none-0, S).

main :-
  start(DL),
  %rb_empty(X), play(0, 1, 9, 25, DL-X, S), _-H = S, highscore(H, _-P),
  rb_empty(X), play(0, 1, 463, 71787, DL-X, S), _-H = S, highscore(H, _-P),
  writeln(P).
