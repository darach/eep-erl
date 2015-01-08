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
-export([start/3]).
-export([new/3]).
-export([new/4]).
-export([new/5]).
-export([tick/1]).
-export([push/2]).

-export([loop/1]).

-record(state, {
    interval :: integer(),
    agg_mod :: module(),
    clock_mod :: module(),
    seed = [] :: list(),
    clock,
    aggregate :: any(),
    callback = undefined :: fun((...) -> any()),
    pid :: pid(),
    epoch :: integer()
}).

start(AggMod, Interval) ->
    start(AggMod, eep_clock_wall, Interval).
start(AggMod, ClockMod, Interval) ->
    {ok, EventPid } = gen_event:start_link(),
    CallbackFun = fun(NewAggregate) ->
        gen_event:notify(
            EventPid,
            {emit, AggMod:emit(NewAggregate)}
        )
    end,
    State = new(AggMod, ClockMod, CallbackFun, Interval),
    spawn(?MODULE, loop, [State]).

new(AggMod, CallbackFun, Interval) ->
    new(AggMod, eep_clock_wall, [], CallbackFun, Interval).

-spec new(AggMod::module(), ClockMod::module(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(AggMod, ClockMod, CallbackFun, Interval) ->
    new(AggMod, ClockMod, [], CallbackFun, Interval).

-spec new(AggMod::module(), ClockMod::module(), Seed::list(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(AggMod, ClockMod, Seed, CallbackFun, Interval) ->
    Clock = ClockMod:new(Interval),
    #state{agg_mod=AggMod, aggregate=AggMod:init(Seed),
           clock_mod=ClockMod, clock=Clock, epoch=ClockMod:at(Clock), seed=Seed, interval=Interval,
           callback=CallbackFun}.

-spec push(#state{}, any()) -> {noop,#state{}} | {emit,#state{}}.
push(State, Event) ->
    accum(State, Event).

%% @private.
loop(#state{pid=EventPid}=State) ->
  receive
    tick ->
      {_,NewState} = tick(State),
      loop(NewState);
    { push, Event } ->
      {_,NewState} = accum(State,Event),
      loop(NewState);
    { add_handler, Handler, Arr } ->
      gen_event:add_handler(EventPid, Handler, Arr),
      loop(State);
    { delete_handler, Handler } ->
      gen_event:delete_handler(EventPid, Handler, []),
      loop(State);
    stop ->
      ok;
    {debug, From} ->
      From ! {debug, State},
      loop(State)
  end.

accum(#state{agg_mod=AggMod, aggregate=Agg}=State,Event) ->
    {noop, State#state{ aggregate=AggMod:accumulate(Agg, Event) }}.

tick(#state{callback=CallbackFun, agg_mod=AggMod, aggregate=Agg,
            clock_mod=CkMod, clock=Clock,
            seed=Seed, epoch=Epoch, interval=Interval}=State) ->
    { Ticked, Tocked } =  CkMod:tick(Clock),
    case Ticked of
        true ->
            case CkMod:tock(Tocked,Epoch) of
                {true, Clock1} ->
                    CallbackFun(Agg),
                    {emit,State#state{aggregate=AggMod:init(Seed),clock=Clock1, epoch=(Epoch+Interval)}};
                {false, _Clock1} ->
                    {noop,State#state{aggregate=AggMod:init(Seed),clock=Tocked, epoch=Epoch}}
            end;
        false ->
            {noop,State#state{clock=Tocked}}
    end.
