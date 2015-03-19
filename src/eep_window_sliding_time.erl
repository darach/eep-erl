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

-spec new(Mod::module(), ClockMod::module(), CallbackFun, Size::integer()) ->
    {CallbackFun, #eep_win{}}
      when CallbackFun :: fun((...) -> any()).
new(Mod, ClockMod, CallbackFun, Size) ->
    new(Mod, ClockMod, [], CallbackFun, Size).

-spec new(Mod::module(), ClockMod::module(), Seed::list(), CallbackFun, Size::integer()) ->
    {CallbackFun, #eep_win{}}
      when CallbackFun :: fun((...) -> any()).
new(Mod, ClockMod, Seed, CallbackFun, Size) ->
    W0 = eep_window:sliding({clock, ClockMod, 1}, Size, Mod, Seed),
    C0 = W0#eep_win.clock,
    L0 = W0#eep_win.log,
    L1 = eep_winlog:tick(eep_clock:at(C0), L0),
    {CallbackFun, W0#eep_win{log=L1}}.

push({CBFun, Win}, Event) ->
    case eep_window:decide([{accumulate, Event}], Win) of
        {noop, Pushed} ->
            {noop, {CBFun, Pushed}};
        {{emit, _}, _} ->
            throw({emission,on,push,for,time,window})
    end.

tick({CBFun, Win}) ->
    case eep_window:tick(Win) of
        {noop, Ticked} -> {noop, {CBFun, Ticked}};
        {{emit, _Emission}, Next} -> %% TODO Might need to also use Emission here?
            {Emissions, WinN} = expire_and_emit(CBFun, Next),
            {Emissions, {CBFun, WinN}}
    end.

expire_and_emit(CBFun, #eep_win{}=Win0) ->
    #eep_win{aggmod=AMod, agg=Agg, size=S, log=Log, clock=C}=Win0,
    At = eep_clock:at(C),
    case eep_winlog:expire(At - S, Log) of
        {[], Current} -> %% Nothing expiring
            {noop, Win0#eep_win{log=Current}};
        {Expiring, Current} ->
            {Compensated, Emissions} = lists:foldl(fun(E, {A0, Ems}) ->
                                              CBFun(A0), %% side effect
                                              A1 = AMod:compensate(A0, E),
                                              {A1, Ems++[{emit, A0}]}
                                      end,
                                      {Agg, []}, Expiring),
            {Emissions, Win0#eep_win{agg=Compensated, log=Current}}
    end.
