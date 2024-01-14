-module(ex11).
-export([rpc/2, create_process/1, append_to_list/1, higherPids/2, lowerPids/2, process/2, setup/0]).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
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
            process(ProcessList, NewCoordinator);
        {getElection, Pid} ->
            Pid ! {ok},
            HigherProcesses = higherPids(self(), ProcessList),
            case HigherProcesses of
                [] ->
                    % No higher PIDs, become the new coordinator
                    io:format("I am the new coordinator: ~p~n", [self()]),
                    lists:foreach(fun(Proc) -> rpc(Proc, {setCoordinator, self()}) end, ProcessList),
                    % Exit the process
                    process(ProcessList, Coordinator);
                _ ->
                    % Wait for responses from higher PIDs
                    case lists:any(
                             fun(Proc) ->
                                 case rpc(Proc, {getElection, self()}) of
                                     {ok} ->
                                         % One process returned {ok}, exit immediately
                                         exit(ok);
                                     _ ->
                                         false
                                 end
                             end,
                             HigherProcesses
                         ) of
                        true ->
                            % One process returned {ok}, exit immediately
                            process(ProcessList, Coordinator);
                        false ->
                            % No process returned {ok}, become the new coordinator
                            io:format("Timeout reached. I am the new coordinator: ~p~n", [self()]),
                            lists:foreach(fun(Proc) -> rpc(Proc, {setCoordinator, self()}) end, ProcessList),
                            % Exit the process
                            process(ProcessList, Coordinator)
                    end
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
                    lists:foreach(fun(Proc) -> rpc(Proc, {setCoordinator, self()}) end, ProcessList),
                    % Exit the process
                    process(ProcessList, Coordinator);
                _ ->
                    % Wait for responses from higher PIDs
                    case lists:any(
                             fun(Proc) ->
                                 case rpc(Proc, {getElection, self()}) of
                                     {ok} ->
                                         % One process returned {ok}, exit immediately
                                         exit(ok);
                                     _ ->
                                         false
                                 end
                             end,
                             HigherProcesses
                         ) of
                        true ->
                            % One process returned {ok}, exit immediately
                            process(ProcessList, Coordinator);
                        false ->
                            % No process returned {ok}, become the new coordinator
                            io:format("Timeout reached. I am the new coordinator: ~p~n", [self()]),
                            lists:foreach(fun(Proc) -> rpc(Proc, {setCoordinator, self()}) end, ProcessList),
                            % Exit the process
                            process(ProcessList, Coordinator)
                    end
            end
    end.

setup() ->
    List = spawn(fun() -> append_to_list([]) end),
    Process1 = create_process(List),
    Process2 = create_process(List),
    Process3 = create_process(List),
    
    timer:sleep(500),  
    % Start an election from Process1 (you can choose a different process if needed)
    Process1 ! {startElection}.