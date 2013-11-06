-module(huffman).
-export([huffman_prob/2]).
-export([list_seek/2]).

%% list_seek - Skips elements until a new one is found
list_seek(Element, Skip) -> case length(Element) > 0 of
	true -> (case hd(Element) == Skip of
		true -> list_seek(tl(Element), Skip);
		false -> Element end);
	false -> Element
end.


%% list_count - Counts how many times an element is repeated
list_count(Element, Start) -> case length(Element) > 1 of
	true -> (case hd(Element) == Start of
		true -> 1 + list_count(tl(Element), Start);
		false -> 0 end);
	false -> 1
end.

%% huffman_prob - Takes a sorted list
huffman_prob(Element, Seek) -> case length(Element) > 0 of
	true -> (case Seek =/= hd(Element) of
		true -> huffman_prob(list_seek(tl(Element), hd(Element)), Seek);
		false -> list_count(Element, hd(Element)) end);
	false -> 0
end.



