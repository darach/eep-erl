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
%% File: eep_window_tumbling.erl. Tumbling aggregate window.
%%
%%--------------------------------------------------------------------
%% @doc
%% <p>A tumbling window. Tumbling windows (here) have a fixed a-priori known size.
%% Example: Tumbling window of size 2 computing sum of values.</p>
%% 
%% <pre>
%% 
%%     t0     t1      (emit)    t2            t3         (emit)    t4
%%   +---+  +---+---+         -...-...-
%%   | 1 |  | 2 | 1 |   &lt;3>   : x : x :
%%   +---+  +---+---+         -...+---+---+   +---+---+            ...
%%                                    | 3 |   | 4 | 3 |    &lt;7>
%%                                    +---+   +---+---+
%% 
%% </pre>
%% 
%% <p>So, we only ever need 1 active window to emit a result for every set of N input events.</p>
%% 
%% <p>This is the only window implementation that actually needs a queue (ring buffer)
%% in this simple embedded event processing engine.</p>
%%
%% <p>You can send the following messages to this window:</p>
%% <ul>
%% <li>{ push, Event } - Push an event to the handler.</li>
%% <li>{ add_handler, Handler, Arr }</li>
%% <li>{ delete_handler, Handler }</li>
%% <li>stop - exits (what did you think it would do?)</li>
%% <li>{debug, From}</li>
%% </ul>
%% @end
%%--------------------------------------------------------------------

-module(eep_window_tumbling).

-include_lib("eep_erl.hrl").

-export([start/2]).

%% @private.
-export([tumble/3]).

%%--------------------------------------------------------------------
%% @doc
%% Start the tumbling window with the given conflation module, Mod, 
%% with the given Size
%% @end
%%--------------------------------------------------------------------

-spec start(Mod::module(), Size::integer()) -> pid().

start(Mod, Size) ->
    {ok, EventPid } = gen_event:start_link(),
    spawn(?MODULE, tumble, [Mod, Size, EventPid]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------

-spec tumble(Mod::module(), Size::integer(), EventPid::pid()) -> ok.

tumble(Mod, Size, EventPid) ->
    tumble(Mod, Size, EventPid, 1, apply(Mod, init, [])).

tumble(Mod, Size, EventPid, Count, State) ->
    receive
	{ push, Event } ->
	    NewState = apply(Mod, accumulate, [State, Event]),
	    case Count >= Size of
		false ->
		    tumble(Mod, Size, EventPid, Count+1, NewState);
		true ->		   
		    gen_event:notify(EventPid, {emit, apply(Mod, emit, [NewState])}),
		    tumble(Mod, Size, EventPid, 1, apply(Mod, init, []))
	    end;
	{ add_handler, Handler, Arr } ->
	    gen_event:add_handler(EventPid, Handler, Arr),
	    tumble(Mod, Size, EventPid, Count, State);
	{ delete_handler, Handler } ->
	    gen_event:delete_handler(EventPid, Handler),
	    tumble(Mod, Size, EventPid, Count, State);
	stop ->
	    ok;
	{debug, From} ->
	    From ! {debug, {Mod, Size, Count, State}},
	    tumble(Mod, Size, EventPid, Count, State)
    end.

%%--------------------------------------------------------------------
%% 
%% EUnit tests
%% 
%%--------------------------------------------------------------------

-ifdef(TEST).

basic_test() ->
    Pid = start(eep_stats_count, 2),
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
	{ debug, Debug0 } -> {eep_stats_count, _, _, 0} = Debug0
    end,
    Pid ! {push, baz},
    Pid ! {debug, self()},
    receive
	{ debug, Debug1 } -> {eep_stats_count, _, _, 1} = Debug1
    end,
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
	{ debug, Debug2 } -> {eep_stats_count, _, _, 1} = Debug2
    end,
    Pid ! {debug, self()},
    receive
	{ debug, Debug3 } -> {eep_stats_count, _, _, 1} = Debug3
    end,
    Pid ! stop.

-endif.
