-module(huffman).
-export([huffman_prob/2]).
-export([list_seek/2]).

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



