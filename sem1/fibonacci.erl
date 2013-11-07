-module(fibonacci).
-export([fibb/0]).
-export([fib/1]).

loop(N, Fun) ->
	if N == 0 -> ok; true -> Fun(), loop(N-1, Fun) end.

time(N, F) ->
	%% time in µS
	T1 = now(),
	loop(N, F),
	T2 = now(),
	timer:now_diff(T2, T1).

fib(M, N, C, T) -> case C < T of
	true -> fib(N, M + N, C + 1, T);
	false -> N
end.

fib(N) -> fib(0, 1, 0, N - 1).

fibb() ->
	Ls = [8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 30000 ],
	N = 10,
	Bench = fun(L) ->
		T = time(N, fun() -> fib(L) end),
		io:format("n: ~8w  fib(n) calculated in: ~8w us~n", [L, T])
		end,
	lists:foreach(Bench, Ls).

