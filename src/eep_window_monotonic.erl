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

-export([loop/1]).

-record(state, {
    interval :: integer(),
    mod :: module(),
    seed = [] :: list(),
    clock_mod :: module(),
    clock,
    aggregate :: any(),
    callback = undefined :: fun((...) -> any()),
    pid = undefined :: pid() | undefined
}).

%%--------------------------------------------------------------------
%% @doc
%% Start the monotonic window with the given conflation module, Mod, the
%% given clock module, ClockMod, and the given clock Interval.
%% @end
%%--------------------------------------------------------------------

-spec start(Mod::module(), ClockMod::module(), Interval::integer()) -> pid().

start(Mod, ClockMod, Interval) ->
    {ok, EventPid } = gen_event:start_link(),
    CallbackFun = fun(NewAggregate) ->
        gen_event:notify(
            EventPid,
            {emit, Mod:emit(NewAggregate)}
        )
    end,
    {_, Clock} = ClockMod:tick(ClockMod:new(Interval)),
   spawn(?MODULE, loop, [#state{mod=Mod, clock_mod=ClockMod, clock=Clock, pid=EventPid, aggregate=Mod:init(), callback=CallbackFun}]).

-spec new(Mod::module(), ClockMod::module(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(Mod, ClockMod, CallbackFun, Interval) ->
    {_, Clock} = ClockMod:tick(ClockMod:new(Interval)),
    #state{mod=Mod, clock_mod=ClockMod, clock=Clock, aggregate=Mod:init(), callback=CallbackFun}.

-spec new(Mod::module(), Seed::list(), ClockMod::module(), CallbackFun::fun((...) -> any()), Integer::integer()) -> #state{}.
new(Mod, Seed, ClockMod, CallbackFun, Interval) ->
    {_, Clock} = ClockMod:tick(ClockMod:new(Interval)),
    #state{mod=Mod, seed=Seed, clock_mod=ClockMod, clock=Clock, aggregate=Mod:init(Seed), callback=CallbackFun}.

-spec push(#state{}, any()) -> {noop,#state{}} | {emit,#state{}}.
push(State, Event) ->
    accum(State,Event).
   
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

accum(#state{mod=Mod, clock_mod=ClockMod, clock=Clock, aggregate=Agg}=State,Event) ->
    {noop, State#state{
        mod=Mod,
        clock=ClockMod:inc(Clock),
        aggregate=Mod:accumulate(Agg, Event)
    }}.

tick(#state{mod=Mod, seed=Seed, aggregate=Agg, clock_mod=ClockMod, clock=Clock, callback=CallbackFun}=State) ->
	{ Ticked, Tocked } =  ClockMod:tick(Clock),
	case Ticked of
		true ->
		    case ClockMod:tock(Tocked) of
			{true, Clock1} ->
                CallbackFun(Agg),
                {emit,State#state{aggregate=Mod:init(Seed),clock=Clock1}};
			{false, _Clock1} ->
                {noop,State#state{aggregate=Mod:init(Seed),clock=Tocked}}
		    end;
		false ->
		    {noop,State}
	end.
