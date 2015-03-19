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
%% File: eep_window_sliding.erl. Sliding aggregate window.
%%
%% -------------------------------------------------------------------

-module(eep_window_sliding).

-include_lib("eep_erl.hrl").

-export([start/2]).
-export([new/3]).
-export([new/4]).
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

start(Mod, Size) ->
  {ok, EventPid } = gen_event:start_link(),
  CallbackFun = fun(NewAggregate) ->
      gen_event:notify(
          EventPid,
          {emit, Mod:emit(NewAggregate)}
      )
  end,
  State = new(Mod, CallbackFun, Size),
  spawn(eep_window, loop, [?MODULE, EventPid, State]).

-spec new(Mod::module(), CallbackFun::fun((...) -> any()), Size::integer()) -> #state{}.
new(Mod, CallbackFun, Size) ->
    {CallbackFun, eep_window:sliding(event, Size, Mod, [])}.

-spec new(Mod::module(), Seed::list(), CallbackFun::fun((...) -> any()), Size::integer()) -> #state{}.
new(Mod, Seed, CallbackFun, Size) ->
    {CallbackFun, eep_window:sliding(event, Size, Mod, Seed)}.

push({CBFun, Win}, Event) ->
    case eep_window:decide([{accumulate, Event}, tick], Win) of
        {noop, Pushed} ->
            {noop, {CBFun, Pushed}};
        {{emit, _}, #eep_win{count=C, size=S}=Pushed}
          when C =< S ->
            {noop, {CBFun, Pushed}};
        {{emit, Emission}, Pushed} ->
            CBFun(Emission),
            {emit, {CBFun, eep_window:compensate(Pushed)}}
    end.

tick({_,_}=Win) -> {noop, Win}.
