-module(ex11).
-export([rpc/2, create_process/1, append_to_list/1, higherPids/2, lowerPids/2, process/2, setup/0, send_set_coordinator_messages/2, send_elections/2]).

rpc(Pid, Request) ->
    io:format("sending RPC to : ~p~n", [Pid]),
    Pid ! Request,
    receive
        {Pid, Response} ->
            io:format("I received: ~p~n", [Response]),
            Response
    after 250 ->
        unreachable
    end.

create_process(ProcessList) ->
    NewProcess = spawn(fun() -> process([], undefined) end),
    ProcessList ! {append, NewProcess},
    NewProcess.

append_to_list(ProcessList) ->
    receive
        {append, Pid} ->
            UpdatedList = [Pid | ProcessList],
            lists:foreach(fun(Proc) -> Proc ! {setProcessList, UpdatedList} end, UpdatedList),
            io:format("Current Process List: ~p~n", [UpdatedList]),
            append_to_list(UpdatedList)
    end.

higherPids(HigherThan, ProcessList) ->
    lists:filter(fun(Pid) -> Pid > HigherThan end, ProcessList).

lowerPids(LowerThan, ProcessList) ->
    lists:filter(fun(Pid) -> Pid < LowerThan end, ProcessList).

process(ProcessList, Coordinator) ->
    receive
        {setProcessList, NewProcessList} ->
            io:format("new List: ~p~n", [NewProcessList]),
            process(NewProcessList, Coordinator);

        {setCoordinator, NewCoordinator} ->
            io:format("new Coordinator: ~p~n", [NewCoordinator]),
            process(ProcessList, NewCoordinator);

        {getElection, Pid} ->
            io:format("I have received an election from: ~p~n", [Pid]),
            Pid ! {ok},
            HigherProcesses = higherPids(self(), ProcessList),
            case HigherProcesses of
                [] ->
                    % No higher PIDs, become the new coordinator
                    io:format("I am the new coordinator: ~p~n", [self()]),
                    send_set_coordinator_messages(ProcessList, self()), % Send messages to other processes
                    process(ProcessList, Coordinator);

                _ ->
                    % Wait for responses from higher PIDs
                    send_elections(HigherProcesses, ProcessList),
                    process(ProcessList, Coordinator)
            end;

        {startElection} ->
            SelfPid = self(),
            io:format("Self PID: ~p~n", [SelfPid]),
            io:format("My List of processes: ~p~n", [ProcessList]),
            HigherProcesses = higherPids(SelfPid, ProcessList),
            io:format("Higher Processes: ~p~n", [HigherProcesses]),
            case HigherProcesses of
                [] ->
                    % No higher PIDs, become the new coordinator
                    io:format("I am the new coordinator: ~p~n", [self()]),
                    send_set_coordinator_messages(ProcessList, self()), % Send messages to other processes
                    process(ProcessList, Coordinator);

                _ ->
                    % Wait for responses from higher PIDs
                    send_elections(HigherProcesses, ProcessList),
                    process(ProcessList, Coordinator)
            end
    end.

send_elections([], _) ->
    ok;

send_elections([Proc | Rest], ProcessList) ->
    io:format("my name is: ~p~n", [self()]),
    case rpc(Proc, {getElection, self()}) of
        {ok} ->
            ok;

        unreachable ->
            % Process didn't respond in time (unreachable)
            send_elections(Rest, ProcessList)
    end.

send_set_coordinator_messages([], _) ->
    ok;

send_set_coordinator_messages([Proc | Rest], Coordinator) ->
    Proc ! {setCoordinator, Coordinator},
    send_set_coordinator_messages(Rest, Coordinator).

setup() ->
    List = spawn(fun() -> append_to_list([]) end),
    Process1 = create_process(List),
    Process2 = create_process(List),
    Process3 = create_process(List),

    timer:sleep(500),

    Process1 ! {startElection},

    exit(Process3, kill),

    timer:sleep(500),

    Process2 ! {startElection},

    NewProcess1 = create_process(List),
    NewProcess2 = create_process(List),

    timer:sleep(500),

    NewProcess1 ! {startElection}.

