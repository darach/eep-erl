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
-export([new/3]).
-export([push/2]).

%% @private.
-export([loop/1]).

-record(state, {
    size :: integer(),
    mod :: module(),
    aggregate :: any(),
    callback = undefined :: fun((...) -> any()),
    count = 1 :: integer(),
    pid :: pid()
}).

%%--------------------------------------------------------------------
%% @doc
%% Start the tumbling window with the given conflation module, Mod, 
%% with the given Size
%% @end
%%--------------------------------------------------------------------

-spec start(Mod::module(), Size::integer()) -> pid().

start(Mod, Size) ->
    {ok, EventPid } = gen_event:start_link(),
    CallbackFun = fun(NewAggregate) -> 
        gen_event:notify(
            EventPid,
            {emit, apply(Mod, emit, [NewAggregate])}
        )
    end,
    spawn(?MODULE, loop, [#state{mod=Mod, size=Size, pid=EventPid, count=1, aggregate=apply(Mod,init,[]), callback=CallbackFun}]).

%%
%%
%%
-spec new(Mod::module(), CallbackFun::fun((...) -> any()), Size::integer()) ->#state{}.
new(Mod, CallbackFun, Size) ->
    #state{size = Size, mod = Mod, callback = CallbackFun, aggregate=apply(Mod, init, [])}. 

%%
%%
%%
-spec push(#state{}, any()) -> {noop,#state{}} | {emit,#state{}}.
push(State, Event) ->
    tumble(State, Event).
    
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------

-spec loop(State::#state{}) -> ok.
loop(#state{pid=EventPid}=State) ->
    receive
	{ push, Event } ->
        {_,NewState} = tumble(State,Event),
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

tumble(#state{mod=Mod, size=Size, aggregate=Aggregate,count=Count,callback=CallbackFun}=State,Event) ->
	NewAggregate = apply(Mod, accumulate, [Aggregate, Event]),
	case Count >= Size of
	false -> {noop,State#state{aggregate=NewAggregate,count=Count+1}};
	true ->	
        CallbackFun(NewAggregate),
        {emit,State#state{aggregate=apply(Mod, init, []),count=1}}
	end.

