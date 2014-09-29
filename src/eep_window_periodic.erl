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
-export([new/3]).
-export([new/4]).
-export([tick/1]).
-export([push/2]).

-export([loop/1]).

-record(state, {
    interval :: integer(),
    mod :: module(),
    seed = [] :: list(),
    clock,
    aggregate :: any(),
    callback = undefined :: fun((...) -> any()),
    pid :: pid(),
    epoch :: integer()
}).

start(Mod, Interval) ->
    {ok, EventPid } = gen_event:start_link(),
    CallbackFun = fun(NewAggregate) ->
        gen_event:notify(
            EventPid,
            {emit, Mod:emit(NewAggregate)}
        )
    end,

  {_, Clock} = eep_clock_wall:tick(eep_clock_wall:new(Interval)),
  spawn(?MODULE, loop, [#state{mod=Mod, clock=Clock, pid=EventPid, aggregate=Mod:init(), callback=CallbackFun, epoch=eep_clock_wall:ts()}]).

-spec new(Mod::module(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(Mod, CallbackFun, Interval) ->
    {_, Clock} = eep_clock_wall:tick(eep_clock_wall:new(Interval)),
    #state{mod=Mod, clock=Clock, aggregate=Mod:init(), callback=CallbackFun, epoch=eep_clock_wall:ts()}.

-spec new(Mod::module(), Seed::list(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(Mod, Seed, CallbackFun, Interval) ->
    {_, Clock} = eep_clock_wall:tick(eep_clock_wall:new(Interval)),
    #state{mod=Mod, seed=Seed, clock=Clock, aggregate=Mod:init(Seed), callback=CallbackFun, epoch=eep_clock_wall:ts()}.

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
      gen_event:delete_handler(EventPid, Handler),
      loop(State);
    stop ->
      ok;
    {debug, From} ->
      From ! {debug, State},
      loop(State)
  end.

accum(#state{mod=Mod, clock=Clock, aggregate=Agg}=State,Event) ->
    {noop, State#state{
        mod=Mod,
        clock=eep_clock_wall:inc(Clock),
        aggregate=Mod:accumulate(Agg, Event)
    }}.

tick(#state{mod=Mod, seed=Seed, aggregate=Agg,clock=Clock,callback=CallbackFun,epoch=Epoch}=State) ->
    { Ticked, Tocked } =  eep_clock_wall:tick(Clock),
    case Ticked of
        true ->
            case eep_clock_wall:tock(Tocked,Epoch) of
                {true, Clock1} ->
                    CallbackFun(Agg),
                    {emit,State#state{aggregate=Mod:init(Seed),clock=eep_clock_wall:inc(Clock1), epoch=eep_clock_wall:ts()}};
                {false, _Clock1} ->
                    {noop,State#state{aggregate=Mod:init(Seed),clock=Tocked, epoch=Epoch}}
            end;
        false ->
            {noop,State}
    end.
    
