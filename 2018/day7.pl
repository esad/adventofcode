#!/usr/bin/env swipl -O -t main --quiet

:- consult(shared).

:- use_module(library(dcg/basics), [alpha_to_lower//1]).

line(step(X, Y)) --> 
  `Step `, alpha_to_lower(X), ` must be finished before step `, alpha_to_lower(Y), ` can begin.`.

parse(Line, Step) :- string_codes(Line, C), phrase(line(Step), C).

next(Done, X) :- step(X, _), \+step(_, X), \+member(X, Done).
next(Done, X) :- step(_, X), foreach(step(D, X), member(D, Done)), \+member(X, Done).

is_step(X) :- step(X, _).
is_step(X) :- step(_, X).

path([]).
path([S | Done]) :-
  path(Done),
  bagof(X, next(Done, X), Nexts), sort(Nexts, [S | _]).

% - Part 2

step_duration(S, D) :- D is 60 + (S - 96).

initial_workers([0-none, 0-none, 0-none, 0-none, 0-none]).

wip([], _) :- false.
wip([_-State | _], State).
wip([_-State2 | Rest], State) :- dif(State, State2), wip(Rest, State).

second(S-_, S).

next_available_second(Workers, S) :-
  aggregate(min(S), (maplist(second, Workers, Seconds), member(S, Seconds), dif(S, 0)), S).

assign_work(_, _, [], [], []).
assign_work(Second, Todo, Done, [Work | WS], [Assignment | WS2]) :-
  Work = (Second2 - Job),
  (Second2 =< Second 
    ->
      % Worker is free
      (Job = none -> Done = Done2 ; Done = [Job | Done2]),
      (Todo = [Step | RestSteps] ->
        % We have something to assign
        step_duration(Step, D), End is Second + D, Assignment = End-Step, 
        assign_work(Second, RestSteps, Done2, WS, WS2)
      ;
        % Nothing to assign
        Assignment = 0-none,
        assign_work(Second, Todo, Done2, WS, WS2)
      )
    ;
      % Worker not free, try next
      Assignment = Work, assign_work(Second, Todo, Done, WS, WS2)
  ).

state([s(0, Seen, W0)]) :-
  initial_workers(WI),
  bagof(X, next([], X), Nexts), sort(Nexts, Steps),
  assign_work(0, Steps, Seen, WI, W0).

state([s(S1, Seen, Workers) | SS]) :-
  state(SS),
  [s(_, Seen0, Workers0) | _] = SS,
  next_available_second(Workers0, S1),
  assign_work(S1, [], DoneS1a, Workers0, Workers1a),
  append(Seen0, DoneS1a, Seen0DoneS1a),
  (bagof(X, next(Seen0DoneS1a, X), Nexts), sort(Nexts, Steps)
    ->
      exclude(wip(Workers1a), Steps, RemainingSteps),
      assign_work(S1, RemainingSteps, DoneS1b, Workers1a, Workers),
      append(Seen0DoneS1a, DoneS1b, Seen)
  ;
    % No more next steps
    Seen = Seen0DoneS1a, Workers = []
  ).
  
% -

main :-
  input(Lines),
  maplist(parse, Lines, Steps),
  assert_all(Steps),
  setof(X, is_step(X), AllSteps),
  path(Solution),
  length(Solution, SL), length(AllSteps, L), SL = L,
  reverse(Solution, Reverse),
  string_codes(Str, Reverse), string_upper(Str, Part1),
  writeln(Part1),
  state([s(Seconds, _, []) | _]),
  writeln(Seconds).
  
