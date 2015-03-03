%% -------------------------------------------------------------------
%% Copyright (c) 2013 Darach Ennis < darach at gmail dot com >,
%%                    Michael Coles < michael dot coles at gmail dot com >
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
%% File: eep_window_sliding_time.erl. Sliding aggregate window by time.
%%
%% -------------------------------------------------------------------

-module(eep_window_sliding_time).

-include_lib("eep_erl.hrl").

-export([new/4]).
-export([new/5]).
-export([push/2]).
-export([tick/1]).

-record(state, {
    size :: integer(),
    mod :: module(),
    seed = [] :: list(),
    aggregate :: any(),
    callback = undefined :: fun((...) -> any()),
    count = 1 :: integer(),
    prior = [] :: list()
}).

-spec new(Mod::module(), ClockMod::module(), CallbackFun::fun((...) -> any()), Size::integer()) -> #state{}.
new(Mod, ClockMod, CallbackFun, Size) ->
    new(Mod, [], ClockMod, CallbackFun, Size).

-spec new(Mod::module(), Seed::list(), ClockMod::module(), CallbackFun::fun((...) -> any()), Size::integer()) -> #state{}.
new(Mod, Seed, ClockMod, CallbackFun, Size) ->
    {CallbackFun, eep_window:sliding({clock, ClockMod, Size}, 1, Mod, Seed)}.

push({CBFun, Win}, Event) ->
    case eep_window:push(Event, Win) of
        {noop, Pushed} ->
            {noop, {CBFun, Pushed}};
        {{emit, _}, _} ->
            throw({emission,on,push,for,time,window})
    end.

tick({CBFun, Win}) ->
    case eep_window:tick(Win) of
        {noop, Ticked} -> {noop, {CBFun, Ticked}};
        {{emit, Emission}, Next} ->
            CBFun(Emission),
            {emit, {CBFun, Next}}
    end.
