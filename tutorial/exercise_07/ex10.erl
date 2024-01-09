-module(ex10).
-export([start/1, clock/4, get_time/1, ticker/2, pause_clock/1, resume_clock/1, timer/2, start_timer/2, time_server/1, time_server_loop/2]).

start(Speed) ->
  Clock_Pid = spawn(fun() -> clock(0, Speed, self(), running) end),
  TickerPid = spawn(fun() -> ticker(Clock_Pid, Speed) end),
  Clock_Pid ! {set_Ticker, TickerPid},
  Clock_Pid.

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
      LocalTime3 = Current + Speed,
      Cutc = {MegaSecs, Secs, MicroSecs},
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
  ClockPid = start(Speed),
  spawn(fun() -> time_server_loop(ClockPid, []) end).

time_server_loop(ClockPid, State) ->
  receive
    {show, Pid} ->
      Pid ! {time_server_state, State},
      time_server_loop(ClockPid, State);
    {get, Pid} ->
        time = ClockPid ! {get, self()},
        Pid ! {time},
        time_server_loop(ClockPid, State);
    _Other ->
      io:format("Unknown message received by Time Server.~n"),
      time_server_loop(ClockPid, State)
  end.

client() ->
  spawn(fun() -> client_loop(ClockPid, []) end),

client_loop(LocalTime, ) ->
  receive
    show ->
      io:format("Client State: ~p~n", [State]), % Add your client state here
      client_loop();
    adjust ->
        LocalTime = erlang:localtime(),
        {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
        CutcBeforeAdjustment = {MegaSecs, Secs, MicroSecs},
  
        TimeServerPid ! {adjust, self()},
    _Other ->
      io:format("Unknown message received by Client.~n"),
      client_loop()
  end.

