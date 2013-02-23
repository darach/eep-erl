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
%% File: eep_stats_vars.erl. An aggregate function that computes sample variance
%%
%% -------------------------------------------------------------------

-module(eep_stats_vars).

-include_lib("eep_erl.hrl").

-behaviour(eep_aggregate).

%% aggregate behaviour.
-export([init/0]).
-export([accumulate/2]).
-export([compensate/2]).
-export([emit/1]).

-record(eep_stats_vars, {
  m2 = 0 :: number(),
  m = 0 :: number(),
  d = 0 :: number(),
  n = 0 :: number()
}).

init() ->
  #eep_stats_vars{}.

accumulate(State,X) ->
   N = State#eep_stats_vars.n + 1,
   D = X - State#eep_stats_vars.m,
   M = (D / N) + State#eep_stats_vars.m,
   M2 = State#eep_stats_vars.m2 + D*(X - M),
   State#eep_stats_vars{m2 = M2, m = M, d = D, n = N}.

compensate(State,X) ->
   N = State#eep_stats_vars.n - 1,
   D = State#eep_stats_vars.m - X,
   M = State#eep_stats_vars.m + (D / N),
   M2 = D*(X - M) + State#eep_stats_vars.m2,
   State#eep_stats_vars{m2 = M2, m = M, d = D, n = N}.

emit(State) ->
  State#eep_stats_vars.m2 / (State#eep_stats_vars.n - 1).

-ifdef(TEST).

basic_test() ->
  S0 = init(),
  V0 = S0#eep_stats_vars.m,
  ?assertEqual(0, V0),
  S1 = accumulate(S0, 1),
  ?assertEqual(1.0, S1#eep_stats_vars.m),
  S2 = accumulate(S1, 8),
  ?assertEqual(4.5, S2#eep_stats_vars.m),
  ?assertEqual(24.5, emit(S2)),
  S3 = accumulate(S2, 1),
  ?assertEqual(16.3333333333333332, emit(S3)),
  S4 = accumulate(S3, 2),
  ?assertEqual(11.3333333333333334, emit(S4)),
  S5 = compensate(S4, 2),
  ?assertEqual(16.3333333333333332, emit(S5)).

-endif.
