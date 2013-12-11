%% More like surrdurr

-module(server).
-export([init/1, start/1, stop/0]).

reply({{get, URI, _}, _, _}) ->
	timer:sleep(40),
	http:ok("Arne\n" ++ URI).


request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of
		{ok, String} ->
			gen_tcp:send(Client, reply(http:parse_request(String)));
		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,
	gen_tcp:close(Client).


handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			request(Client),
			handler(Listen);
		{error, Error} ->
			error
	end.


init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			handler(Listen),
			gen_tcp:close(Listen),
			ok;
		{error, Error} ->
			error
	end.

start(Port) ->
	register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
	exit(whereis(rudy), "time to die").
