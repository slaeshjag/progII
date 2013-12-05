-module(philosopher).
-export([start/6]).

start(Hungry, Left, Right, Name, Ctrl, Seed) ->
	spawn_link(fun() -> run(Hungry, Left, Right, Name, Ctrl, Seed) end).

sleep(T, D) -> timer:sleep(T + random:uniform(D)).

run(Hungry, Left, Right, Name, Ctrl, {S1, S2, S3}) ->
	random:seed(S1, S2, S3),
	thinking(Hungry, Left, Right, Name, Ctrl).


thinking(0, _, _, Name, Ctrl) -> 
	io:format("~s is now done~n", [Name]), Ctrl ! done, done;

thinking(Hungry, Left, Right, Name, Ctrl) ->
	%% Think for a while %%
	sleep(100, 5),
	%% Eat a little %%
	chopstick:request(Left, 100),
	io:format("~s got a chopstick~n", [Name]),
	chopstick:request(Right, 100),
	sleep(100, 50),
	chopstick:return(Right),
	chopstick:return(Left),
	io:format("~s gets back to thinking~n", [Name]),
	thinking(Hungry - 1, Left, Right, Name, Ctrl).

