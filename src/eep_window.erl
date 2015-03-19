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
%% File: eep_window.erl. Generic window-based functionality.
%%
%% -------------------------------------------------------------------
-module(eep_window).

-export([start/3]).
-export([start/4]).

-export([loop/3]).

-export([tumbling/4]).
-export([sliding/4]).
-export([tick/1]).

%% DEBUG
-export([accumulate/2, compensate/1, reset/1, decide/2, decide/3, tick_/1]).

-include("eep_erl.hrl").

%% Pure window functionality.

%% Window type creation functions.
tumbling({clock, Clock, Interval}, Size, Aggregate, Seed) ->
    #eep_win{type=tumbling, by=time, compensating=false,
         size=Size, clockmod=Clock, clock=(Clock:new(Interval)),
         seed=Seed, aggmod=Aggregate, agg=(Aggregate:init(Seed))};
tumbling(event, Size, Aggregate, Seed) ->
    W = tumbling({clock, eep_clock_count, Size}, Size, Aggregate, Seed),
    W#eep_win{by=event}. %% TODO FIXME

sliding({clock, Clock, Interval}, Size, Aggregate, Seed) ->
    #eep_win{type=sliding, by=time, compensating=true,
         size=Size, clockmod=Clock, clock=(Clock:new(Interval)),
         seed=Seed, aggmod=Aggregate, agg=(Aggregate:init(Seed))};
sliding(event, Size, Aggregate, Seed) ->
    W = sliding({clock, eep_clock_count, 1}, Size, Aggregate, Seed),
    W#eep_win{by=event}. %% TODO FIXME

%% Window command interface.

%% A thin wrapper that allows to stop external parties ticking a by-event window.
%% TODO THis might be unnecessary - maybe we don't need to make this restriction?
tick(#eep_win{by=event}) ->
    error({how,can,you,tick,that,which,is,untickable});
tick(#eep_win{by=time, count=C}=Win) ->
    %% TODO FIXME This count increment doesn't feel right here - there must be
    %% a more elegant solution!
    {Actions, TickedWin} = tick_(Win#eep_win{count=C+1}),
    decide(Actions, TickedWin).

tick_(#eep_win{}=Win) ->
    #eep_win{log=Log, clockmod=CkMod, clock=Clock} = Win,
    {Actions, Win2} =
        case eep_clock:tick(CkMod, Clock) of
            {noop, UnTicked} ->
                {[], Win#eep_win{clock=UnTicked}};
            {tock, Tocked} ->
                {[emit], Win#eep_win{clock=Tocked}}
        end,
    TickedLog =
        if Win#eep_win.compensating ->
               Curr = eep_clock:at(Win2#eep_win.clock),
               eep_winlog:tick(Curr, Log);
           true -> Log
        end,
    {Actions, Win2#eep_win{log=TickedLog}}.

%% Window internal mechanisms: these correspond to the actions decided upon
%% by the behaviour functions above.
%% Accumulate a single event: if we are compensating, add it to the current buffer.
accumulate(Event, #eep_win{aggmod=AMod, agg=Agg, count=Count, log=Log}=Win) ->
    Accumed = Win#eep_win{agg=(AMod:accumulate(Agg, Event)), count=Count+1},
    if Win#eep_win.compensating -> %% If we're to compensate we must store the events
           Accumed#eep_win{log=(eep_winlog:append(Event, Log))};
       true ->
           Accumed
    end.

reset(#eep_win{compensating=false}=Win) ->
    #eep_win{aggmod=AMod, seed=Seed}=Win,
    Win#eep_win{agg=(AMod:init(Seed)), count=1}.

compensate(#eep_win{compensating=true}=Win) ->
    #eep_win{aggmod=AMod, agg=Agg, size=S, log=Log, clock=C}=Win,
    At = eep_clock:at(C),
    {Expiring, Current} = eep_winlog:expire(At - S, Log),
    % Compensated = lists:foldl(fun AMod:compensate/2, Agg, Expiring),
    Compensated = lists:foldl(fun(E, A) -> AMod:compensate(A, E) end,
                              Agg, Expiring),
    Win#eep_win{agg=Compensated, log=Current}.

%% Given a list of actions (from the behaviour functions above), an event and
%% the current window state, apply the actions in order and return the result.
%% Its implementation is ~equivalent to lists:foldl/3 on the list of actions.
decide(Actions, Window) ->
    decide(Actions, Window, noop).

decide([], Window, Decision) -> {Decision, Window};
decide([ tick |Actions], Window, Decision) ->
    {MoreActions, TdWin} = tick_(Window),
    decide(MoreActions++Actions, TdWin, Decision);
decide([ {accumulate, Event} |Actions], Window, Decision) ->
    decide(Actions, accumulate(Event, Window), Decision);
decide([ compensate |Actions], Window, Decision) ->
    decide(Actions, compensate(Window), Decision);
%decide([ emit |Actions], #eep_win{count=C, size=S}=Window, noop)
%  when C =< S -> %% TODO FIXME count < size -> skip emission 
%    decide(Actions, Window, noop);
decide([ emit |Actions], #eep_win{}=Window, noop) ->
    #eep_win{size=Size, clock=C0}=Window,
    %% We need to check the clock for total
    %% elapsed time: if it's less than Size, skip the emission
    Elapsed = eep_clock:elapsed(C0),
    if Elapsed < Size -> decide(Actions, Window, noop);
       true -> decide(Actions, Window, {emit, Window#eep_win.agg})
    end.

%% Process functionality: utils for running windows as a process.
start(Window, AggMod, Interval) ->
    start(Window, AggMod, eep_clock_wall, Interval).
start(Window, AggMod, ClockMod, Interval) ->
    {ok, EventPid } = gen_event:start_link(),
    CallbackFun = fun(NewAggregate) ->
        gen_event:notify(
            EventPid,
            {emit, AggMod:emit(NewAggregate)}
        )
    end,
    State = Window:new(AggMod, ClockMod, CallbackFun, Interval),
    spawn(?MODULE, loop, [Window, EventPid, State]).

loop(Window, EventPid, State) ->
  receive
    tick ->
      {_,NewState} = Window:tick(State),
      loop(Window, EventPid, NewState);
    { push, Event } ->
      {_,NewState} = Window:push(State,Event),
      loop(Window, EventPid, NewState);
    { add_handler, Handler, Arr } ->
      gen_event:add_handler(EventPid, Handler, Arr),
      loop(Window, EventPid, State);
    { delete_handler, Handler } ->
      gen_event:delete_handler(EventPid, Handler, []),
      loop(Window, EventPid, State);
    stop ->
      ok;
    {debug, From} ->
      From ! {debug, State},
      loop(Window, EventPid, State)
  end.
