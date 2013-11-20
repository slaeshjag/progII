-module(huffman).
-export([huffman_prob/2]).
-export([list_seek/2]).
-export([table/1]).

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

table(Data) -> get_tree(lists:sort(Data)).
