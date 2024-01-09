-module(ex10).
-export([start/1, clock/4, get_time/1, ticker/2, pause_clock/1, resume_clock/1, timer/2, start_timer/2, time_server/1, time_server_loop/2, client/2, client_loop/3, adjuster/0]).

start(Speed) ->
  ClockPid = spawn(fun() -> clock(0, Speed, self(), running) end),
  TickerPid = spawn(fun() -> ticker(ClockPid, Speed) end),
  ClockPid ! {set_Ticker, TickerPid},
  ClockPid.

ticker(ClockPid, Speed) ->
  ClockPid ! {tick, self()},
  timer:sleep(Speed),
  ticker(ClockPid, Speed).

pause_clock(ClockPid) ->
  ClockPid ! pause.

resume_clock(ClockPid) ->
  ClockPid ! resume.

clock(Current, Speed, TickerPid, Status) ->
  receive
    {set_Ticker, T_Pid} ->
      clock(Current, Speed, T_Pid, Status);

    {set, Value} ->
      clock(Value, Speed, TickerPid, Status);
    {get, Pid} ->
      LocalTime2 = Current,
      {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
      Cutc = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
      LocalTime3 = Current + Speed,
      Pid ! {clock, {LocalTime2, LocalTime3, Cutc}},
      clock(Current, Speed, TickerPid, Status);
    pause ->
      clock(Current, Speed, TickerPid, paused);
    resume ->
      clock(Current, Speed, TickerPid, running);
    stop ->
      exit(finished);

    {tick, Sender} when Sender == TickerPid ->
      case Status of
        running ->
          NewTime = Current + Speed,
          clock(NewTime, Speed, TickerPid, running);
        paused -> clock(Current, Speed, TickerPid, Status)
      end;
    {tick, _} ->
      clock(Current, Speed, TickerPid, Status)
  end.

get_time(ClockPid) ->
  ClockPid ! {get, self()},
  receive
    {clock, Time} ->
      Time
  end.

start_timer(Ticks, Func) ->
  TimerPid = spawn(fun() -> timer(Ticks, Func) end),
  spawn(fun() -> ticker(TimerPid, 1000) end).

timer(0, Func) ->
  Func();
timer(Ticks, Func) ->
  receive
    {tick, _} -> timer(Ticks - 1, Func)
  end.

time_server(Speed) ->
  ClockPid = spawn(fun() -> clock(0, Speed, self(), running) end),
  TickerPid = spawn(fun() -> ticker(ClockPid, Speed) end),
  ClockPid ! {set_Ticker, TickerPid},
  ServerPid = spawn(fun() -> time_server_loop(ClockPid, []) end),
  ServerPid.

time_server_loop(ClockPid, State) ->
  receive
    {show, Pid} ->
      Pid ! {time_server_state, State},
      time_server_loop(ClockPid, State);
    {get, Pid} ->
      ClockPid ! {get, self()},
      receive
        {clock, {T2, T3, Cutc}} ->
          Pid ! {T2, T3, Cutc},
          time_server_loop(ClockPid, State)
      end;
    _Other ->
      io:format("Unknown message received by Time Server.~n"),
      time_server_loop(ClockPid, State)
  end.

client(Speed, TimeServerPid) ->
  ClockPid = spawn(fun() -> clock(0, Speed, self(), running) end),
  TickerPid = spawn(fun() -> ticker(ClockPid, Speed) end),
  ClockPid ! {set_Ticker, TickerPid},
  ClientPid = spawn(fun() -> client_loop(TimeServerPid, ClockPid, []) end),
  io:format("Adjustment started ~p~n", [ClientPid]),
  ClientPid.

client_loop(TimeServerPid, ClockPid, State) ->
  receive
    show ->
      io:format("Client State: ~p~n", [State]),
      client_loop(TimeServerPid, ClockPid, State);
    adjust ->

      TimeServerPid ! {get, self()},
      ClockPid ! {get, self()},
      receive
        {T2, T3, Cutc} ->

          T1 = 0,
          T4 = 0,
          ClockPid ! {set, (T2 - T1 + T4 - T3) / 2 + Cutc},
          AdjustedTime = (T2 - T1 + T4 - T3) / 2 + Cutc,
          io:format("Adjusted Time: ~p~n", [AdjustedTime]),
          client_loop(TimeServerPid, ClockPid, State)
      end;
    _Other ->
      client_loop(TimeServerPid, ClockPid, State)
  end.

adjuster() ->

  ClockPidServer = spawn(fun() -> clock(0, 1, self(), running) end),
  TickerPidServer = spawn(fun() -> ticker(ClockPidServer, 1) end),
  ClockPidServer ! {set_Ticker, TickerPidServer},
  ServerPid = spawn(fun() -> time_server_loop(ClockPidServer, []) end),


  ClockPidClient1 = spawn(fun() -> clock(2000, 2, self(), running) end),
  TickerPidClient1 = spawn(fun() -> ticker(ClockPidClient1, 1) end),
  ClockPidClient1 ! {set_Ticker, TickerPidClient1},
  ClientPid1 = spawn(fun() -> client_loop(ServerPid, ClockPidClient1, []) end),


  ClockPidClient2 = spawn(fun() -> clock(0, 1, self(), running) end),
  TickerPidClient2 = spawn(fun() -> ticker(ClockPidClient2, 1) end),
  ClockPidClient2 ! {set_Ticker, TickerPidClient2},
  ClientPid2 = spawn(fun() -> client_loop(ServerPid, ClockPidClient2, []) end),


  ClockPidClient3 = spawn(fun() -> clock(1000, 0.0001, self(), running) end),
  TickerPidClient3 = spawn(fun() -> ticker(ClockPidClient3, 1) end),
  ClockPidClient3 ! {set_Ticker, TickerPidClient3},
  ClientPid3 = spawn(fun() -> client_loop(ServerPid, ClockPidClient3, []) end),

  io:format("Adjustment started ~n"),
  ClientPid1 ! adjust,
  ClientPid2 ! adjust,
  ClientPid3 ! adjust.


  

