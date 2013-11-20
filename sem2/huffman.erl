-module(huffman).
-export([huffman_prob/2]).
-export([list_seek/2]).
-export([table/1]).
-export([encode/2]).
-export([decode/2]).
-export([test/0]).

sample() -> "the quick brown fox jumps over the lazy dog this is a sample text that we will use when we build up a table we will only handle lower case letters and no punctuation symbols the frequency will of course not represent english but it is probably not that far off".
%sample() -> "En stor rund hund med mycket jobbiga tecken, som kan drastiskt minska kompressionsgraden av text. Räksmörgås. Vad drar du för slutsats av detta? Allt jag vet är att huffmankodningen i sig verkar fungera rätt okej".


%% list_seek - Skips elements until a new one is found
list_seek([], _) -> [];
list_seek([H|T], Skip) -> case H == Skip of
	true -> list_seek(T, Skip);
	false -> [H|T]
end.


%% list_count - Counts how many times an element is repeated
list_count([], _) -> 0;
list_count([H|T], Start) -> case H == Start of
	true -> 1 + list_count(T, Start);
	false -> 0
end.

%% huffman_prob - Takes a sorted list
huffman_prob([], _) -> 0;
huffman_prob([H|T], Seek) -> case Seek =/= H of
	true -> huffman_prob(list_seek(T, H), Seek);
	false -> list_count([H|T], H) 
end.


make_list(_, 256) -> [];
make_list(Data, Byte) -> [{byte, Byte, huffman_prob(Data, Byte)}|make_list(Data, Byte + 1)].

make_tree(TreeStublets, _, 1) -> TreeStublets;
make_tree([], TreeStublets, 0) -> make_tree(TreeStublets, [], length(TreeStublets));
make_tree([G,H|Tail], TreeStublets, _) -> case length(Tail) == 0 of 
	false -> make_tree(Tail, make_tree([G,H], TreeStublets, length([G,H|Tail])), length(Tail));
	true -> make_tree(Tail, lists:keysort(3, [{branch, padding, element(3, G) + element(3, H), G, H} | TreeStublets]), length(Tail))
end.

make_leaf_list({byte, Byte, Freq}) -> {leaf, Byte, Freq}.
make_leaf_list([G|Tail], List) -> make_leaf_list(Tail, [make_leaf_list(G)|List]);
make_leaf_list([], N) -> N.


get_tree(SortedData) -> L = lists:sort(make_leaf_list(make_list(SortedData, 0), [])), make_tree(lists:keysort(3, L), [], length(L)).

get_table([{branch, padding, _, L, R}|_], History) -> get_table([L], History ++ [0]) ++ get_table([R], History ++ [1]);
get_table([{leaf, Byte, Freq}], History) -> [{table, Byte, Freq, History}].


table(Data) -> N = get_tree(lists:sort(Data)), T = get_table(N, []), {endec, N, T}.


%% Encode! %%

find_encoding(Element, [{table, Element, _, Encoding}|_]) -> Encoding;
find_encoding(Element, [_|T]) -> find_encoding(Element, T).

encode([], _) -> [];
encode([H|T], {endec, _, Ta}) -> find_encoding(H, Ta) ++ encode(T, {endec, 0, Ta}).

%% Decode %%

find_decoding([0|T], {branch, _, _, L, _}, Otr) -> find_decoding(T, L, Otr);
find_decoding([1|T], {branch, _, _, _, R}, Otr) -> find_decoding(T, R, Otr);
find_decoding(T, {leaf, Byte, _}, Otr) -> [Byte|find_decoding(T, Otr, Otr)];
find_decoding([], _, _) -> [].

decode(T, {endec, [Tr], _}) -> find_decoding(T, Tr, Tr).


%% Test %%

test() -> Sample = sample(),
	Table = table(Sample),
	Seq = encode(Sample, Table),
	Text = decode(Seq, Table),
	io:format("Before: ~w bits, after: ~w bits~n Compression ratio: ~w~n", [length(Text) * 8, length(Seq), length(Seq) / length(Text) / 8]),
	Text.
