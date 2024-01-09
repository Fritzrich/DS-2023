-module(ex9).
-export([start/1, clock/4, get_time/1, ticker/2, pause_clock/1, resume_clock/1, timer/2, start_timer/2]).

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
      Pid ! {clock, Current},
      clock(Current, Speed, TickerPid, Status);
    pause ->
      clock(Current, Speed, TickerPid, paused);
    resume ->
      clock(Current, Speed, TickerPid, running);
    stop ->
      exit(finished);

    {tick, Sender} when Sender == TickerPid->
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


