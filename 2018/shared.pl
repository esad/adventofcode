input(R) :-
  read_string(user_input, _, S),
  split_string(S, "\n", "", L),
  exclude(=(""), L, R).

assert_all([]).
assert_all([H | T]) :- assert(H), assert_all(T).
