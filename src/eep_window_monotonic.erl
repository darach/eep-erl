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
%% File: eep_window_monotonic.erl. Monotonic window.
%%
%%--------------------------------------------------------------------
%% @doc
%% A monotonic window. Client should call tick monotonically and use with a monotonic clock implementation.
%% @end
%%--------------------------------------------------------------------

-module(eep_window_monotonic).

-include_lib("eep_erl.hrl").

-export([start/3]).
-export([new/4]).
-export([new/5]).
-export([tick/1]).
-export([push/2]).

-record(state, {
    interval :: integer(),
    agg_mod :: module(),
    seed = [] :: list(),
    clock_mod :: module(),
    clock,
    aggregate :: any(),
    callback = undefined :: fun((...) -> any())
}).

start(Mod, ClockMod, Interval) ->
    eep_window:start(?MODULE, Mod, ClockMod, Interval).


new(Mod, eep_clock_count, CallbackFun, Interval) ->
    new(Mod, [], eep_clock_count, CallbackFun, Interval).

new(Mod, Seed, eep_clock_count, CallbackFun, Interval) ->
    {CallbackFun,
     eep_window:tumbling({clock, eep_clock_count, Interval}, 1, Mod, Seed)}.

-spec push(#state{}, any()) -> {noop,#state{}} | {emit,#state{}}.
push({CBFun, W0}, Event) ->
    case eep_window:decide([{accumulate, Event}], W0) of
        {{emit, Em}, W1} ->
            CBFun(Em),
            {emit, {CBFun, eep_window:reset(W1)}};
        {noop, W1} -> {noop, {CBFun, W1}}
    end.

tick({CBFun, W0}) ->
    case eep_window:tick(W0) of
        {noop, W1} -> {noop, {CBFun, W1}};
        {{emit, Em}, W1} ->
            CBFun(Em),
            {emit, {CBFun, eep_window:reset(W1)}}
    end.
