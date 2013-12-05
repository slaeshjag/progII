-module(dinner).
-export([start/0]).
-export([init/0]).

start() -> 
	spawn(fun() -> init() end).

init() ->
	C1 = chopstick:start(),
	C2 = chopstick:start(),
	C3 = chopstick:start(),
	C4 = chopstick:start(),
	C5 = chopstick:start(),
	Ctrl = self(),
	philosopher:start(5, C1, C2, "Arendt", Ctrl, {545, 545, 427}),
	philosopher:start(5, C2, C3, "Hypatia", Ctrl, {323, 323, 227}),
	philosopher:start(5, C3, C4, "Simone", Ctrl, {232, 232, 3227}),
	philosopher:start(5, C4, C5, "Elizabeth", Ctrl, {121, 121, 2127}),
	philosopher:start(5, C5, C1, "Ayn", Ctrl, {676, 676, 7627}),

	T1 = now(),
	wait(5, [C1, C2, C3, C4, C5]),
	T2 = now(),
	io:format("Time for all philosophers to finish: ~w~n", [timer:now_diff(T2,T1)]).



wait(0, Chopsticks) ->
	lists:foreach(fun(C) -> chopstick:quit(C) end, Chopsticks);
wait(N, Chopsticks) ->
	receive
		done -> wait(N-1, Chopsticks);
		abort -> exit(abort)
	end.


