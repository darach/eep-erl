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

-export([tick/2]).

-callback name() ->
    Name :: atom().
-callback at(ck_state()) ->
    Now :: integer().
-callback new(Interval :: integer()) ->
    ck_state().
-callback inc(Old :: ck_state()) ->
    New :: ck_state().
-callback tick(Old :: ck_state()) ->
    {Tocked :: boolean(), New :: ck_state()}.
-callback tock(Old :: ck_state()) ->
    {Tocked :: boolean(), New :: ck_state()}.

-spec tick(module(), Curr :: ck_state()) ->
    {noop, UnTicked :: ck_state()}
    | {tock, Tocked :: ck_state()}.
tick(CkMod, Clock) ->
    case CkMod:tick(Clock) of
        {false, UnTicked} -> {noop, UnTicked};
        {true, Ticked} ->
            {_, Tocked} = CkMod:tock(Ticked),
            {tock, Tocked}
    end.
