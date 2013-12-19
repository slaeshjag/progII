%% More like surrdurr

-module(server).
-export([init/0]).

-define(Opt, [binary, {packet, 0}, {reuseaddr, true}, {active, true}, {nodelay, true}]).
-define(Port, 8080).
-define(Header, [{'icy-name', "ID1019"}, {'icy-genre', "Groove"}, {'icy-notice1', "Our own jukebox"}]).
-define(Timeout, 1000000).


parser(<<13, 10, 13, 10>>, start) -> ok;
parser(<<Bin/binary>>, start) when size(Bin) < 4 -> moar;
parser(<<_, Rest/binary>>, start) -> parser(Rest, start).
parser(Segment) ->
	case parser(Segment, start) of
		ok -> {ok, Segment, []};
		moar -> {moar, fun(More) -> parser(<<Segment/binary, More/binary>>) end}
	end.


reader(Parser, Socket) ->
	receive
		{tcp, Socket, More} ->
			case Parser(More) of
				{ok, Parsed, Rest} ->
					{ok, Parsed, Rest};
				{moar, Cont} ->
					reader(fun(More) -> Cont(More) end, Socket);
				{error, Error} ->
					{error, Error}
			end;
		{tcp_closed, Socket} ->
			{error, "Socket closed"};
		stop ->
			ok
	after ?Timeout ->
		gen_tcp:close(Socket),
		{error, "Connection timeout"}
	end.


reader(Socket) ->
	reader(fun(More) -> parser(More) end, Socket).

loop(_, [], _) ->
	ok;
loop(Header, [{seg, Segment}|Rest], Socket) ->
	%io:format("Sending segment~n", []),
	gen_tcp:send(Socket, Segment),
	%io:format("Sending header~n", []),
	gen_tcp:send(Socket, Header),
	loop(Header, Rest, Socket).


server(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	io:format("Server: connect~n", []),
	case reader(Socket) of
		{ok, Request, _} ->
			io:format("server: received request ~p~n", [Request]),

			%% Get resource, load it
			{ok, _, {Resource, _, _, _}} = icy:parse_request(Request),
			{mp3, Title, Data} = mp3:read_file("." ++ Resource),
			Header = icy:encode_meta([{title, Title}]),
			Segments = icy:segments(Data),

			gen_tcp:send(Socket, icy:encode_response(Request)),
			loop(Header, Segments, Socket),
			gen_tcp:close(Socket),
			io:format("server: disconnect~n"),
			server(Listen);
		{error, Error} ->
			io:format("server: ~s~n", [Error])
	end.


init() ->
%	{mp3, Title, Data} = mp3:read_file(File),
%	Header = icy:encode_meta([{title, Title}]),
%	Segments = icy:segments(Data),
	{ok, Listen} = gen_tcp:listen(?Port, ?Opt),
	server(Listen).


