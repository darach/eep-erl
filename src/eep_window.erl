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
%% File: eep_window.erl. Generic window-based functionality.
%%
%% -------------------------------------------------------------------
-module(eep_window).

-export([start/3]).
-export([start/4]).

-export([loop/3]).

start(Window, AggMod, Interval) ->
    start(Window, AggMod, eep_clock_wall, Interval).
start(Window, AggMod, ClockMod, Interval) ->
    {ok, EventPid } = gen_event:start_link(),
    CallbackFun = fun(NewAggregate) ->
        gen_event:notify(
            EventPid,
            {emit, AggMod:emit(NewAggregate)}
        )
    end,
    State = Window:new(AggMod, ClockMod, CallbackFun, Interval),
    spawn(?MODULE, loop, [Window, EventPid, State]).

loop(Window, EventPid, State) ->
  receive
    tick ->
      {_,NewState} = Window:tick(State),
      loop(Window, EventPid, NewState);
    { push, Event } ->
      {_,NewState} = Window:push(State,Event),
      loop(Window, EventPid, NewState);
    { add_handler, Handler, Arr } ->
      gen_event:add_handler(EventPid, Handler, Arr),
      loop(Window, EventPid, State);
    { delete_handler, Handler } ->
      gen_event:delete_handler(EventPid, Handler, []),
      loop(Window, EventPid, State);
    stop ->
      ok;
    {debug, From} ->
      From ! {debug, State},
      loop(Window, EventPid, State)
  end.
