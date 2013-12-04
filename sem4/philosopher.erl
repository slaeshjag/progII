-module(philosopher).
-export([start/5]).

start(Hungry, Left, Right, Name, Ctrl) ->
	spawn_link(fun() -> thinking(Hungry, Left, Right, Name, Ctrl) end).

sleep(T, D) -> timer:sleep(T + random:uniform(D)).


thinking(0, _, _, Name, Ctrl) -> 
	io:format("~s is now done~n", [Name]), Ctrl ! done, done;

thinking(Hungry, Left, Right, Name, Ctrl) ->
	%% Think for a while %%
	sleep(200, 100),
	%% Eat a little %%
	chopstick:request(Left),
	io:format("~s got a chopstick~n", [Name]),
	chopstick:request(Right),
	sleep(100, 50),
	chopstick:return(Right),
	chopstick:return(Left),
	io:format("~s gets back to thinking~n", [Name]),
	thinking(Hungry - 1, Left, Right, Name, Ctrl).

