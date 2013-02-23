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
%% File: eep_stats_avg.erl. An aggregate function that computes averages
%%
%% -------------------------------------------------------------------

-module(eep_stats_avg).

-include_lib("eep_erl.hrl").

-behaviour(eep_aggregate).

%% aggregate behaviour.
-export([init/0]).
-export([accumulate/2]).
-export([compensate/2]).
-export([emit/1]).

-record(eep_stats_avg, { 
  m = 0 :: number(), 
  d = 0 :: number(), 
  n = 0 :: number()
}).

init() ->
  #eep_stats_avg{}.

accumulate(State,X) ->
   N = State#eep_stats_avg.n + 1,
   D = X - State#eep_stats_avg.m,
   M = (D / N) + State#eep_stats_avg.m,
   State#eep_stats_avg{m = M, d = D, n = N}.

compensate(State,X) ->
   N = State#eep_stats_avg.n - 1,
   D = State#eep_stats_avg.m - X,
   M = State#eep_stats_avg.m + (D / N),
   State#eep_stats_avg{m = M, d = D, n = N}.

emit(State) ->
  State#eep_stats_avg.m.

-ifdef(TEST).

basic_test() ->
  S0 = init(),
  V0 = S0#eep_stats_avg.m,
  ?assertEqual(0, V0),
  S1 = accumulate(S0, 1),
  ?assertEqual(1.0, S1#eep_stats_avg.m),
  S2 = accumulate(S1, 8),
  ?assertEqual(4.5, S2#eep_stats_avg.m),
  ?assertEqual(4.5, emit(S2)),
  S3 = compensate(S2, 3), % technically cheating / illegal usage - but legal ...
  ?assertEqual(6.0, emit(S3)).

-endif.
