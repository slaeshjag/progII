-module(moves).
-export([move/2]).


single({one, N}, {P, O, T}) ->
	case (N > 0) of
		true -> {list:take(P, length(P) - N), list:append(list:drop(P, length(P) - N), O), T};
		false -> M = N * -1, {list:append(P, list:take(O, M)), list:drop(O, M), T}
	end;

single({two, N}, {P, O, T}) ->
	case (N > 0) of
		true -> {list:take(P, length(P) - N), O, list:append(list:drop(P, length(P) - N), T)};
		false -> M = N * -1, {list:append(P, list:take(T, M)), O, list:drop(T, M)}
	end.

move([], S) -> [S];
move([H|T], S) -> Ns = single(H, S), [S|move(T, Ns)].


