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

-export([new/3]).
-export([new/4]).
-export([new/5]).
-export([tick/1]).
-export([push/2]).

-record(state, {
    interval :: integer(),
    agg_mod :: module(),
    clock_mod :: module(),
    seed = [] :: list(),
    clock,
    aggregate :: any(),
    callback = undefined :: fun((...) -> any())
}).

new(AggMod, CallbackFun, Interval) ->
    new(AggMod, eep_clock_wall, [], CallbackFun, Interval).

-spec new(AggMod::module(), ClockMod::module(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(AggMod, ClockMod, CallbackFun, Interval) ->
    new(AggMod, ClockMod, [], CallbackFun, Interval).

-spec new(AggMod::module(), ClockMod::module(), Seed::list(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(AggMod, ClockMod, Seed, CallbackFun, Interval) ->
    Clock = ClockMod:new(Interval),
    #state{agg_mod=AggMod, aggregate=AggMod:init(Seed), seed=Seed,
           clock_mod=ClockMod, clock=Clock, interval=Interval,
           callback=CallbackFun}.

-spec push(#state{}, any()) -> {noop,#state{}} | {emit,#state{}}.
push(State, Event) ->
    accum(State, Event).

accum(#state{agg_mod=AggMod, aggregate=Agg}=State,Event) ->
    {noop, State#state{ aggregate=AggMod:accumulate(Agg, Event) }}.

tick(#state{callback=CallbackFun, agg_mod=AggMod, aggregate=Agg, seed=Seed,
            clock_mod=CkMod, clock=Clock}=State) ->
    case eep_clock:tick(CkMod, Clock) of
        {noop, Clock2} -> {noop, State#state{clock=Clock2}};
        {tock, Tocked} ->
            CallbackFun(Agg),
            {emit, State#state{aggregate=AggMod:init(Seed), clock=Tocked}}
    end.
