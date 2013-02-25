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

%% @private
-export([tock/4]).

%%--------------------------------------------------------------------
%% @doc
%% Start the monotonic window with the given conflation module, Mod, the
%% given clock module, ClockMod, and the given clock Interval.
%% @end
%%--------------------------------------------------------------------

-spec start(Mod::module(), ClockMod::integer(), Interval::integer()) -> pid().

start(Mod, ClockMod, Interval) ->
    {ok, EventPid } = gen_event:start_link(),
    {_, Clock} = apply(ClockMod,tick,[apply(ClockMod,new, [Interval])]),
    spawn(?MODULE, tock, [Mod, ClockMod, Clock, EventPid]).

%% @private
tock(Mod, ClockMod, Clock, EventPid) ->
    tock(Mod, ClockMod, Clock, EventPid, apply(Mod, init, [])).

%% @private
tock(Mod, ClockMod, Clock, EventPid, State) ->
    receive
	tick ->
	    { Ticked, Tocked } =  apply(ClockMod,tick,[Clock]),
	    case Ticked of
		true ->
		    case apply(ClockMod, tock, [Tocked, apply(ClockMod,at,[Tocked])]) of
			{true, Clock1} ->
			    gen_event:notify(EventPid, {emit, apply(Mod, emit, [State])}),
			    tock(Mod, ClockMod, Clock1, EventPid, apply(Mod, init, []));
			{false, _Clock1} ->
			    tock(Mod, ClockMod, Tocked, EventPid, State)
		    end;
		false ->
		    tock(Mod, ClockMod, Clock, EventPid, State)
	    end;
	{ push, Event } ->
	    tock(Mod, ClockMod, apply(ClockMod,inc,[Clock]), EventPid, apply(Mod, accumulate, [State, Event]));
	{ add_handler, Handler, Arr } ->
	    gen_event:add_handler(EventPid, Handler, Arr),
	    tock(Mod, ClockMod, Clock, EventPid, State);
	{ delete_handler, Handler } ->
	    gen_event:delete_handler(EventPid, Handler),
	    tock(Mod, ClockMod, Clock, EventPid, State);
	stop ->
	    ok;
	{debug, From} ->
	    From ! { debug, {Mod, Clock, EventPid, State}},
	    tock(Mod, ClockMod, Clock, EventPid, State)
    end.

%%--------------------------------------------------------------------
%% 
%% EUnit tests
%% 
%%--------------------------------------------------------------------

-ifdef(TEST).

basic_test() ->
    Pid = start(eep_stats_count, eep_clock_count, 0),
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
	{ debug, Debug0 } -> {eep_stats_count, _, _, 2} = Debug0
    end,
    Pid ! tick,
    Pid ! {debug, self()},
    receive
	{ debug, Debug1 } -> {eep_stats_count, _, _, 0} = Debug1
    end,
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
	{ debug, Debug2 } -> {eep_stats_count, _, _, 6} = Debug2
    end,
    Pid ! tick,
    Pid ! {debug, self()},
    receive
	{ debug, Debug3 } -> {eep_stats_count, _, _, 0} = Debug3
    end,
    Pid ! stop.

-endif.
