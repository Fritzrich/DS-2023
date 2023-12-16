-module(ex8).
-export([echo/0, collector/2, start/0, pipeline/2]).

echo() ->
    receive
        {list, ListFromCollector} ->
            io:format("I have received something~n"),
            io:format("Echo: Received list from collector: ~p~n", [ListFromCollector]),
        echo();
        stop -> ok
    end.

collector(To, List) ->
    receive
        {reset} ->
            io:format("Collector: Resetting\n"),
            collector(To, []);
        {Msg} ->
            NewList = List ++ [Msg],
            io:format("Collector: Adding to list: {~p}\n", [Msg]),
            io:format("~p~n\n", [NewList]),
            collector(To, NewList);
        {setsender, Pid} ->
            io:format("Sending To: {~p}\n" , [Pid]),
            Pid ! {list, List},  % Send the list to Echo
            io:format("SenT To Echo\n"),
            collector(To, List);
        _ -> collector(To, List)
    end.


pipeline(To, Forward) ->
    receive
        {filter, Msg} when Forward == true ->
            To ! {Msg},
            pipeline(To, false);
        {filter, Msg} when Forward == false ->
            pipeline(To, true);
        {filter, reset} ->
            To ! {reset};
        {setsender, Pid} ->
            To ! {setsender, Pid},
            pipeline(To, Forward);
        _ ->
            pipeline(To, Forward)
    end.

start() ->
    Echo = spawn(?MODULE, echo, []),
    case is_process_alive(Echo) of
        true ->
            io:format("Echo process is alive.~n");
        false ->
            io:format("Echo process is not alive.~n")
        end,
    C = spawn(?MODULE, collector, [Echo, []]),  % Fix this line

    P2 = spawn(?MODULE, pipeline, [C, false]),
 

    P2 ! {filter, 120},
    P2 ! {filter, 109},
    P2 ! {filter, 150},
    P2 ! {filter, 101},
    P2 ! {filter, 155},
    P2 ! {filter, 114},
    P2 ! {filter, 189},
    P2 ! {filter, 114},
    P2 ! {filter, 27},
    P2 ! {filter, 121},
    P2 ! {filter, 68},
    P2 ! {filter, 32},
    P2 ! {filter, 198},
    P2 ! {filter, 99},
    P2 ! {filter, 33},
    P2 ! {filter, 104},
    P2 ! {filter, 164},
    P2 ! {filter, 114},
    P2 ! {filter, 212},
    P2 ! {filter, 105},
    P2 ! {filter, 194},
    P2 ! {filter, 115},
    P2 ! {filter, 24},
    P2 ! {filter, 116},
    P2 ! {filter, 148},
    P2 ! {filter, 109},
    P2 ! {filter, 173},
    P2 ! {filter, 97},
    P2 ! {filter, 8},
    P2 ! {filter, 115},
    P2 ! {filter, 191},
    P2 ! {filter, 33},

    case is_process_alive(Echo) of
            true ->
                io:format("Echo process is alive.~n");
            false ->
                io:format("Echo process is not alive.~n")
            end,

    P2 ! {setsender, Echo},
    P2 ! {filter, reset},

    ok.
