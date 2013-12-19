-module(mp3).

id3_scrub([0|_]) -> [];
id3_scrub([C|Rest]) ->
	[C|id3_scrub(Rest)].

id3_title({id3, [{title, Title}|_]}) -> Title.

id3_decode_year(<<Year:4, _>>) ->
	[{year, id3_scrub(Year)}].

id3_decode_album(<<Album:30, Rest/binary>>) ->
	[{album, id3_scrub(Album)} | id3_decode_year(Rest)].

id3_decode_artist(<<Artist:30, Rest/binary>>) ->
	[{artists, id3_scrub(Artist)} | id3_decode_album(Rest)].

id3_decode_title(<<Title:30, Rest/binary>>, Size) ->
	{ok, {id3, [{title, id3_scrub(Title)} | id3_decode_artist(Rest)]}, Size}.
	

id3_decode_head(<<$T, $A, $G, Data/binary>>, Size) ->
	id3_decode_title(Data, Size);
id3_decode_head(_, Size) ->
	{ok, na, Size}.


id3_tag(S, Size) ->
	Data = file:pread(S, Size-128, 128),
	id3_decode_head(Data, Size).


read_file(File) ->
	Size = filelib:file_size(File),
	{ok, S} = file:open(File, [read, binary, raw]),
	{ok, Id3, End} = id3_tag(S, Size),
	{ok, Data} = file:pread(S, 0, End),
	Title = id3_title(Id3),
	{mp3, Title, Data}.

