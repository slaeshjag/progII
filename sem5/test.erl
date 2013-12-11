-module(test).
-export([bench/2]).

bench(Host, Port) ->
	Start = now(),
	run(100, Host, Port),
	Finnish = now(),
	timer:now_diff(Finnish, Start).

run(0, Host, Port) ->
	ok;
run(N, Host, Port) ->
	request(Host, Port),
	run(N-1, Host, Port).

request(Host, Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	{ok, Server} = gen_tcp:connect(Host, Port, Opt),
	gen_tcp:send(Server, http:get("foo")),
	Recv = gen_tcp:recv(Server, 0),
	case Recv of
		{ok, _} ->
			ok;
		{error, Error} ->
			io:format("test: Error: ~w~n", [Error])
	end.
