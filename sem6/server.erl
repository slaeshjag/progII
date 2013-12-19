%% More like surrdurr

-module(server).
-export([init/1, start/1, stop/0]).

reply({{get, URI, _}, _, _}) ->
	case file:read_file("/tmp" ++ URI) of
		{ok, Reply} -> http:ok(binary_to_list(Reply));
		{error, Error} -> http:err("404 File not found")
	end.


reader(Parser, Socket) ->
	receive
		{tcp, Socket, More} ->
			case Parser(More) of
				{ok, Parsed, Rest} ->
					Parsed;
				{moar, Cont} ->
					reader(fun(More) -> Cont(More) end, Socket);
				{error, Error} ->
					{error, Error}
			end;
		{tcp_closed, Socket} ->
			{error, "Socket closed"};
		stop ->
			ok
	end.


reader(Socket) ->
	reader(fun() -> parser(<<>>) end, Socket).

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
	Opt = [list, {active, true}, {reuseaddr, true}],
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
