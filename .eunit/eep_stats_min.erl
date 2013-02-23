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
%% File: eep_stats_min.erl. Min aggregate function.
%%
%% -------------------------------------------------------------------

-module(eep_stats_min).


-include_lib("eep_erl.hrl").

-behaviour(eep_aggregate).

%% aggregate behaviour.
-export([init/0]).
-export([accumulate/2]).
-export([compensate/2]).
-export([emit/1]).

init() ->
  ?EEP_MAX.

accumulate(State,X) ->
  case State =< X of true -> State; false -> X end.

compensate(State,X) ->
  case State =< X of true -> X; false -> State end.

emit(State) ->
  State.

-ifdef(TEST).

basic_test() ->
  S0 = init(),
  ?assertEqual(?EEP_MAX, S0),
  S1 = accumulate(S0, 1),
  ?assertEqual(1, S1),
  S2 = accumulate(S1, 8),
  ?assertEqual(1, S2),
  ?assertEqual(1, emit(S2)),
  S3 = compensate(S2, 100),
  ?assertEqual(100, emit(S3)).

-endif.
