-module(shunt).
-export([find/2]).

split(L, A) -> Pos = list:position(L, A), {list:take(Pos - 1), list:drop(Pos)}.

find(Src, Target) -> case list:diffpos(Src, Target) =< length(Src) of
	true ->
		Pos = list:diffpos(Src, Target),
		Rem = list:drop(Src, Pos - 1),
		W = lists:nth(Pos, Target),
		V = list:position(Rem, W),
		One = length(Rem) - V + 1,
		Two = V - 1,
		Move = [{one, One}, {two, Two}, {one, One * -1}, {two, Two * -1}],
		State = moves:move(Move, {Src, [], []}),
		Cur = element(1, lists:nth(length(State), State)),
		Move ++ find(Cur, Target);
	false -> []
end.
