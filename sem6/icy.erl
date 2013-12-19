-module(icy).
-export([parse_request/1, segments/1, encode_meta/1, encode_response/1]).
-define(Chunk, 8192).

segments(<<Chunk:?Chunk/binary, Rest/binary>>) ->
	[{seg, Chunk} | segments(Rest)];
segments(<<_/binary>>) ->
	[].



encode_headers([]) -> <<>>;
encode_headers([{Key, Value}|Rest]) ->
	case Key of
		title -> Val = list_to_binary(Value),
		Head = <<"StreamTitle='", Val/binary, "';">>;
		_ -> Head = <<>>
	end,
	RestH = encode_headers(Rest),
	<<Head/binary, RestH/binary>>.


padding(Data, 0) -> {size(Data) div 16, Data};
padding(Data, Pad) -> padding(<<Data/binary, 0>>, Pad - 1).

padding(Data) ->
	padding(Data, 16 - (size(Data) rem 16)).


encode_meta(Headers) ->
	Meta = encode_headers(Headers),
	{K, Padded} = padding(Meta),
	<<K/integer, Padded/binary>>.

encode_header(_) -> "\r\n".


encode_response(Header) ->
	Status = "ICY 200 OK\r\n",
	MetaInt = "icy-metaint: " ++ integer_to_list(?Chunk) ++ "\r\n",
	Reply = Status ++ MetaInt ++ encode_header(Header),
	list_to_binary(Reply).


read_line(<<>>) -> moar;
read_line(<< 13, 10, Rest/binary>>) -> {line, [], Rest};
read_line(Data) when size(Data) == 2 ->
	moar;
read_line(<<C, Rest/binary>>) ->
	case read_line(Rest) of
		{line, Data, Rest2} -> {line, [C | Data], Rest2};
		moar -> moar
	end.

headers(Data, Headers) ->
	case read_line(Data) of
		{line, [], Rest} ->
			{ok, Headers, Rest};
		{line, Head, Rest} ->
			headers(Rest, [Head | Headers]);
		moar -> moar
	end.


decode_version(Segment) when size(Segment) < 10 ->
	moar;
decode_version(<<$H, $T, $T, $P, $/, $1, $., $1, 13, 10, Rest/binary>>) ->
	{ok, v11, Rest};
decode_version(<<$H, $T, $T, $P, $/, $1, $., $0, 13, 10, Rest/binary>>) ->
	{ok, v10, Rest}.


decode_resource(<<32, R0/binary>>, _) ->
	{ok, [], R0};
decode_resource(<<C, R0/binary>>, _) ->
	{ok, Rest, R1} = decode_resource(R0),
	{ok, [C | Rest], R1}.
decode_resource(Data) ->
	case read_line(Data) of
		{line, _, _} -> decode_resource(Data, foo);
		moar -> moar
	end.

request_line(Segment) when size(Segment) < 4 ->
	moar;
request_line(<<$G, $E, $T, 32, R1/binary>>) ->
	case decode_resource(R1) of
		{ok, Resource, R2} ->
			case decode_version(R2) of
				{ok, Version, R3} ->
					{ok, get, Resource, Version, R3};
				moar ->
					moar;
				Other ->
					{error, "strange version: " ++ Other}
			end;
		moar ->
			moar;
		error ->
			{error, "failed to parse resource"}
	end;
request_line(Line) ->
	{error, "not a GET request: " ++ Line}.


parse_request(Segment) ->
	case request_line(Segment) of
		{ok, Method, Resource, Version, R1} ->
			case headers(R1, []) of
				{ok, Headers, Body} ->
					{ok, Method, {Resource, Version, Headers, Body}};
				moar ->
					{moar, fun(More) -> parse_request(<<Segment/binary, More/binary>>) end}
			end;
		{ok, Req, _} ->
			{error, "Invalid request: " ++ Req};
		moar ->
			{moar, fun(More) -> parse_request(<<Segment/binary, More/binary>>) end}
	end.

		
