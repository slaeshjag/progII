-module(huffman).
-export([huffman_prob/2]).
-export([list_seek/2]).

%% list_seek - Skips elements until a new one is found
list_seek(Element, Skip) -> case hd(Element) == Skip of
	true -> list_seek(tl(Element), Skip);
	false -> Element
end.


%% list_count - Counts how many times an element is repeated
list_count(Element, Start) -> case hd(Element) == Start of
	true -> 1 + list_count(tl(Element), Start);
	false -> 0
end.

%% huffman_prob - Takes a sorted list
huffman_prob(Element, Seek) -> case Seek >= 1 of
	true -> huffman_prob(list_seek(tl(Element), hd(Element)), Seek - 1);
	false -> list_count(Element, hd(Element))
end.



