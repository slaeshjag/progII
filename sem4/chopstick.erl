-module(chopstick).
-export([start/0, request/1, return/1, quit/1]).

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
		{quit, Pid} -> Pid ! {quit, ok}, exit(normal)
	end,

	available().

request(Stick) ->
	Stick ! {request, self()},
	receive
		{request, ok} -> granted;
		{request, gone} -> gone
	end.

return(Stick) ->
	Stick ! {return, self()},
	receive
		{return, ok} -> ok
	end.

quit(Stick) ->
	Stick ! {quit, self()},
	receive
		{quit, ok} -> ok
	end.

