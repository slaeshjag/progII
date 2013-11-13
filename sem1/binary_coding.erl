-module(binary_coding).
-export([get_bit/1]).
-export([get_bit2/1]).

get_bit(Num, Pos) -> case Pos =< 1 of
	true -> [];
	false -> (case (Num rem Pos) >= (Pos div 2) of
		true -> [1|get_bit(Num, Pos div 2)];
		false -> [0|get_bit(Num, Pos div 2)]
	end)
end.

find_pos(Num, Pos) -> case Pos =< Num of
	true -> find_pos(Num, Pos * 2);
	false -> get_bit(Num, Pos)
end.


get_bit(Num) -> find_pos(Num, 2).


list_unchr(Lst) -> case Lst == [] of
	true -> [];
	false ->  [hd(Lst) - 48 | list_unchr(tl(Lst))]
end.


get_bit2(Num) -> list_unchr(integer_to_list(Num, 2)).
