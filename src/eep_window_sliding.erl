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

%% private.
-export([loop/1]).

-record(state, {
    size :: integer(),
    mod :: module(),
    seed = [] :: list(),
    aggregate :: any(),
    callback = undefined :: fun((...) -> any()),
    count = 1 :: integer(),
    pid :: pid(),
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
  spawn(?MODULE, loop, [#state{mod=Mod, seed=[], size=Size, pid=EventPid, count=1, aggregate=Mod:init(), callback=CallbackFun}]).

-spec new(Mod::module(), CallbackFun::fun((...) -> any()), Size::integer()) -> #state{}.
new(Mod, CallbackFun, Size) ->
    #state{size=Size, mod=Mod, callback=CallbackFun, aggregate=Mod:init()}.

-spec new(Mod::module(), Seed::list(), CallbackFun::fun((...) -> any()), Size::integer()) -> #state{}.
new(Mod, Seed, CallbackFun, Size) ->
    #state{size=Size, seed=Seed, mod=Mod, callback=CallbackFun, aggregate=Mod:init(Seed)}.


push(State, Event) ->
    slide(State, Event).

-spec loop(State::#state{}) -> ok.
loop(#state{pid=EventPid}=State) ->
  receive
    { push, Event } ->
        {_,NewState} = slide(State,Event),
        loop(NewState);
    { add_handler, Handler, Arr } ->
        gen_event:add_handler(EventPid, Handler, Arr),
        loop(State);
    { delete_handler, Handler } ->
        gen_event:delete_handler(EventPid, Handler),
        loop(State);
    stop ->
      ok;
    {debug, From} ->
      From ! {debug, State},
      loop(State)
  end.

slide(#state{mod=Mod, size=Size, aggregate=Aggregate,count=Count,callback=CallbackFun,prior=Prior}=State,Event) ->
    NewAggregate = Mod:accumulate(Aggregate, Event),
    if 
        Count < Size ->
            NewPrior = Prior ++ [Event],
            {noop,State#state{aggregate=NewAggregate,count=Count+1,prior=NewPrior}};
        Count == Size ->
            NewPrior = Prior ++ [Event],
            CallbackFun(NewAggregate), 
            {emit,State#state{aggregate=NewAggregate,count=Count+1,prior=NewPrior}};
        true ->
            Value = lists:nth(1,Prior),
            CallbackFun(NewAggregate), 
            NewAggregate2 = Mod:compensate(NewAggregate, Value),
            NewPrior = erlang:tl(Prior) ++ [Event],
            {emit,State#state{aggregate=NewAggregate2,count=Count+1,prior=NewPrior}}
    end.
