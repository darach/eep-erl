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

-record(state, {
    interval :: integer(),
    agg_mod :: module(),
    clock_mod :: module(),
    seed = [] :: list(),
    clock,
    aggregate :: any(),
    callback = undefined :: fun((...) -> any())
}).

start(AggMod, Interval) ->
    start(AggMod, eep_clock_wall, Interval).
start(AggMod, ClockMod, Interval) ->
    eep_window:start(?MODULE, AggMod, ClockMod, Interval).

new(AggMod, CallbackFun, Interval) ->
    new(AggMod, eep_clock_wall, [], CallbackFun, Interval).

-spec new(AggMod::module(), ClockMod::module(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(AggMod, ClockMod, CallbackFun, Interval) ->
    new(AggMod, ClockMod, [], CallbackFun, Interval).

-spec new(AggMod::module(),
          ClockMod::module(),
          Seed::list(),
          CallbackFun,
          Integer::integer()) ->
    {CallbackFun, #eep_win{}}
      when CallbackFun :: fun((...) -> any()).
new(AggMod, ClockMod, Seed, CallbackFun, Interval) ->
    {CallbackFun, eep_window:tumbling({clock, ClockMod, Interval}, 1, AggMod, Seed)}.

-spec push(#eep_win{}, any()) -> {noop,#eep_win{}}.
push({CBFun, Window}, Event) ->
    {noop, Pushed} = eep_window:decide([{accumulate, Event}], Window),
    {noop, {CBFun, Pushed}}.

tick({CBFun, Window}) ->
    case eep_window:tick(Window) of
        {noop, Next} -> {noop, {CBFun, Next}};
        {{emit, Emission}, Next} ->
            CBFun(Emission),
            {emit, {CBFun, eep_window:reset(Next)}}
    end.
