-module(list).
-export([take/2]).
-export([drop/2]).
-export([append/2]).
-export([member/2]).
-export([position/2]).

take(Xs, N) -> element(1, lists:split(N, Xs)).
drop(Xs, N) -> element(2, lists:split(N, Xs)).
append(Xs, Ys) -> Xs ++ Ys.

member(Xs, Y) -> case (length(Xs) > length(lists:delete(Y, Xs))) of
	true -> true;
	false -> false
end.

position([Y|_], Y, N) -> N;
position([_|T], Y, N) -> position(T, Y, N + 1).
position([Y|_], Y) -> 0;
position([_|T], Y) -> position(T, Y, 2).


