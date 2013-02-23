%% -------------------------------------------------------------------
%% Copyright (c) 2013 Darach Ennis < darach at gmail dot com > 
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% File: eep_window_periodic.erl. Periodic aggregate window.
%%
%% -------------------------------------------------------------------

-module(eep_window_periodic).

-include_lib("eep_erl.hrl").

-export([start/2]).

%% @private.
-export([tock/3]).

start(Mod, Interval) ->
  {ok, EventPid } = gen_event:start_link(),
  {_, Clock} = eep_clock_wall:tick(eep_clock_wall:new(Interval)),
  spawn(?MODULE, tock, [Mod, Clock, EventPid]).

%% @private.
tock(Mod, Clock, EventPid) ->
  tock(Mod, Clock, EventPid, apply(Mod, init, []), eep_clock_wall:ts()).

%% @private.
tock(Mod, Clock, EventPid, State, Epoch) ->
  receive
    tick ->
      { Ticked, Tocked } =  eep_clock_wall:tick(Clock),
      case Ticked of
        true ->
          case eep_clock_wall:tock(Tocked,Epoch) of
            {true, Clock1} ->
              gen_event:notify(EventPid, {emit, apply(Mod, emit, [State])}),
              tock(Mod, Clock1, EventPid, apply(Mod, init, []), eep_clock_wall:ts());
            {false, _Clock1} ->
              tock(Mod, Tocked, EventPid, State, Epoch)
          end;
        false ->
          tock(Mod, Tocked, EventPid, State, Epoch)
      end;
    { push, Event } ->
      tock(Mod, eep_clock_wall:inc(Clock), EventPid, apply(Mod, accumulate, [State, Event]), Epoch);
    { add_handler, Handler, Arr } ->
      gen_event:add_handler(EventPid, Handler, Arr),
      tock(Mod, Clock, EventPid, State, Epoch);
    { delete_handler, Handler } ->
      gen_event:delete_handler(EventPid, Handler),
      tock(Mod, Clock, EventPid, State, Epoch);
    stop ->
      ok;
    {debug, From} ->
      From ! { debug, {Mod, Clock, EventPid, State}},
      tock(Mod, Clock, EventPid, State, Epoch)
  end.

-ifdef(TEST).

basic_test() ->
  Pid = start(eep_stats_count, 0),
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {debug, self()},
  receive
    { debug, Debug0 } -> {eep_stats_count, _, _, 2} = Debug0
  end,
  Pid ! tick,
  Pid ! {debug, self()},
  receive
     { debug, Debug1 } -> {eep_stats_count, _, _, 0} = Debug1
  end,
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {debug, self()},
  receive
    { debug, Debug2 } -> {eep_stats_count, _, _, 6} = Debug2
  end,
  Pid ! tick,
  Pid ! {debug, self()},
  receive
     { debug, Debug3 } -> {eep_stats_count, _, _, 0} = Debug3
  end,
  Pid ! stop.

-endif.
