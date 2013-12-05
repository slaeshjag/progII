-module(chopstick).
-export([start/0, request/2, return/1, quit/1]).

start() ->
	spawn_link(fun() -> available() end).

gone() ->
	receive
		{return, Pid} -> Pid ! {return, ok};
		{quit, Pid} -> Pid ! {quit, ok}, exit(normal)
	end.


available() ->
	receive
		{request, Pid} -> Pid ! {request, ok}, gone();
		{request2, Pid} -> Pid ! {request2, self()};
		{quit, Pid} -> Pid ! {quit, ok}, exit(normal)
	end,

	available().

request(Stick, Timeout) ->
	Stick ! {request, self()},
	receive
		{request, ok} -> granted;
		{request, gone} -> gone
	%%after Timeout -> gone
	end.

request2(Stick, Timeout) ->
	Stick ! {request2, self()}.

return(Stick) ->
	Stick ! {return, self()},
	receive
		{return, ok} -> ok
	end.

granted(Stick, Stick2, Delay, Stick) ->
	receive
		{request2, Pid} -> {Stick, Pid}
	after Delay -> return(Stick), {0, 0}
	end;
granted(Stick, Stick2, Delay, Stick2) ->
	receive
		{request2, Pid} -> {Stick2, Stick}
	after Delay -> return(Stick2), {0, 0}
	end.

granted(Stick, Stick2, Delay) ->
	receive
		{request2, Pid} -> granted(Stick, Stick2, Pid)
	after Delay -> {0, 0}
	end.


quit(Stick) ->
	Stick ! {quit, self()},
	receive
		{quit, ok} -> ok
	end.

