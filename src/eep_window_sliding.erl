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

%% private.
-export([slide/3]).

start(Mod, Size) ->
  {ok, EventPid } = gen_event:start_link(),
  spawn(?MODULE, slide, [Mod, Size, EventPid]).

slide(Mod, Size, EventPid) ->
  slide(Mod, Size, EventPid, 1, apply(Mod, init, []), []).

slide(Mod, Size, EventPid, Count, State, Prior) ->
  receive
    { push, Event } ->
      case Count >= Size of
        false -> 
          NewState = apply(Mod, accumulate, [State, Event]),
          Prior2 = Prior ++ [Event],
          slide(Mod, Size, EventPid, Count+1, NewState, Prior2);
        true ->
           Value = lists:nth(1,Prior),
		   NewState = apply(Mod, accumulate, [State, Event]),
           gen_event:notify(EventPid, {emit, apply(Mod, emit, [NewState])}),
           NewState2 = apply(Mod, compensate, [NewState, Value]),
           Prior2 = lists:sublist(Prior,2,length(Prior)-1) ++ [Event],
           slide(Mod, Size, EventPid, Count+1, NewState2, Prior2)
      end;
    { add_handler, Handler, Arr } ->
      gen_event:add_handler(EventPid, Handler, Arr),
      slide(Mod, Size, EventPid, Count, State, Prior);
    { delete_handler, Handler } ->
      gen_event:delete_handler(EventPid, Handler),
      slide(Mod, Size, EventPid, Count, State, Prior);
    stop ->
      ok;
    {debug, From} ->
      From ! {debug, {Mod, Size, Count, State}},
      slide(Mod, Size, EventPid, Count, State, Prior)
  end.

-ifdef(TEST).

basic_test() ->
  Pid = start(eep_stats_count, 2),
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {debug, self()},
  receive
    { debug, Debug0 } -> {eep_stats_count, _, _, 1} = Debug0
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
