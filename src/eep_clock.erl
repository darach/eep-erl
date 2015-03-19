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
%% File: eep_clock.erl. Defines clock implementation behaviour.
%%
%% -------------------------------------------------------------------

-module(eep_clock).

-include_lib("eep_erl.hrl").

-export([at/1]).
-export([elapsed/1]).
-export([tick/2]).

-callback name() ->
    Name :: atom().
-callback new(Interval :: integer()) ->
    ck_state().
-callback inc(Old :: ck_state()) ->
    New :: ck_state().

-spec at(ck_state()) ->
    Now :: integer().
at(#eep_clock{at=At}) -> At.

-spec elapsed(ck_state()) ->
    Now :: integer().
elapsed(#eep_clock{origin=Origin, at=At}) ->
    At - Origin.

-spec tick(module(), Curr :: ck_state()) ->
    {noop, UnTicked :: ck_state()}
    | {tock, Tocked :: ck_state()}.
tick(CkMod, Clock0) ->
    Clock1 = CkMod:inc(Clock0),
    #eep_clock{at=At, mark=Mark, interval=Interval}=Clock1,
    if (At - Mark) >= Interval ->
           {_, Tocked} = tock(Clock1),
           {tock, Tocked};
       true -> {noop, Clock1}
    end.

tock(Clock0) ->
  Delta = Clock0#eep_clock.at - Clock0#eep_clock.mark,
  case Delta >= Clock0#eep_clock.interval of
    true -> {true, Clock0#eep_clock{mark = Clock0#eep_clock.mark + Clock0#eep_clock.interval}};
    false -> {false, Clock0}
  end.
