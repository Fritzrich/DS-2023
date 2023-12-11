% Erlang-Module in File bsp.erl 

-module(ex7). 
-export([convert/2]). 
-export([maxitem/1]).
-export([diff/3]).

convert(Amount, Unit) ->
    case Unit of
        inch ->
            {cm, Amount * 2.54};
        cm ->
            {inch, Amount / 2.54}
    end.

maxitem([]) ->
    io:format("Empty list. Returning 0.~n"),
    0;

maxitem(List) when is_list(List) ->
    io:format("Starting maxitem with list: ~p~n", [List]),
    maxitem(List, -2147483648).

maxitem([], Max) ->
    io:format("Reached end of the list. Returning Max: ~p~n", [Max]),
    Max;

maxitem([Head | Tail], Max) when Head > Max ->
    io:format("Updating Max from ~p to ~p~n", [Max, Head]),
    maxitem(Tail, Head);

maxitem([Skip | Tail], Max) ->
    io:format("Skipping element ~p. Current Max: ~p~n", [Skip, Max]),
    maxitem(Tail, Max).

diff(F, X, H) when is_function(F, 1) ->
    (F(X + H) - F(X - H)) / (2 * H).