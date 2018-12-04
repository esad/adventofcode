input(R) :-
  read_string(user_input, _, S),
  split_string(S, "\n", "", L),
  exclude(=(""), L, R).
