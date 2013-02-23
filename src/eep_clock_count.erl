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
%% File: eep_clock_count.erl. Monotonic (simple counting) clock.
%%
%% -------------------------------------------------------------------

-module(eep_clock_count).

-include_lib("eep_erl.hrl").

-behaviour(eep_clock).

%% clock behaviour.
-export([name/0]).
-export([at/1]).
-export([new/1]).
-export([inc/1]).
-export([tick/1]).
-export([tock/2]).

name() -> count.

at(State) -> 
 State#eep_clock.at.

new(Interval) ->
  #eep_clock{at = 0, interval = Interval}.

inc(State) -> 
  State#eep_clock{at = State#eep_clock.at + 1}.

tick(State) ->
  NewState = case State#eep_clock.mark of
    undefined -> State#eep_clock{mark = State#eep_clock.at};
    _Other -> State
  end,
  {(NewState#eep_clock.at - NewState#eep_clock.mark) >= NewState#eep_clock.interval, NewState}.

tock(State, _Elapsed) ->
  Delta = State#eep_clock.at,
  case Delta >= State#eep_clock.interval of
    true -> {true, State#eep_clock{mark = State#eep_clock.mark + State#eep_clock.interval}};
    false -> {false, State}
  end.

-ifdef(TEST).

basic_test() ->
  C0 = new(1),
  Then0 = 0,
  case (Then0 - C0#eep_clock.at) =:= 0 of
    true -> ok
  end,
  C1 = inc(C0),
  case (C1#eep_clock.at - Then0) =:= 1 of
    true -> ok
  end,
  {false,C2} = tick(C1),
  {true,C3} = tock(C2,1),
  ?assertEqual(2, C3#eep_clock.mark).

-endif.
