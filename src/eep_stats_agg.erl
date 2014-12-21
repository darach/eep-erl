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
%% File: eep_stats_agg.erl. Aggregator of aggregates, fanning out work to
%%       eg. eep_stats_sum, eep_stats_count, etc.
%%
%% -------------------------------------------------------------------

-module(eep_stats_agg).

-include_lib("eep_erl.hrl").

-behaviour(eep_aggregate).

%% aggregate behaviour.
-export([init/0]).
-export([init/1]).
-export([accumulate/2]).
-export([compensate/2]).
-export([emit/1]).

% useless to have this empty
init() ->
  init([[]]).

init([]) ->
  init();
init([KeyModSeed]) when is_record(KeyModSeed, dict, 9) ->
  init([dict:to_list(KeyModSeed)]);
init([KeyModSeed]) when is_list(KeyModSeed) ->
  State = [ {K, init_mod(ModSeed)} || {K, ModSeed} <- KeyModSeed ],
  dict:from_list(State).

accumulate(State,{K,V,ModSeed}) ->
  ModState2 = case dict:find(K, State) of
    {ok, {Mod, S}} ->
      {Mod, Mod:accumulate(S, V)};
    error ->
      {Mod, S} = init_mod(ModSeed),
      {Mod, Mod:accumulate(S, V)}
  end,
  dict:store(K, ModState2, State).

compensate(State,{K,V,ModSeed}) ->
  ModState2 = case dict:find(K, State) of
    {ok, {Mod, S}} ->
      {Mod, Mod:compensate(S, V)};
    error ->
      {Mod, S} = init_mod(ModSeed),
      {Mod, Mod:compensate(S, V)}
  end,
  dict:store(K, ModState2, State).

% returns a proplist not a dict...
emit(State) ->
  [ {K, Mod:emit(S)} || {K,{Mod,S}} <- dict:to_list(State) ].

% private
init_mod({Mod, Seed}) -> {Mod, Mod:init([Seed])};
init_mod(Mod) -> {Mod, Mod:init()}.

-ifdef(TEST).

basic_test() ->
  AggMapping = [{a, {eep_stats_count, 10}}, {b, eep_stats_sum}],
  S0 = eep_stats_agg:init([AggMapping]),
  S1 = eep_stats_agg:accumulate(S0, {a, 20, not_needed_mod}),
  S2 = eep_stats_agg:accumulate(S1, {b, 20, not_needed_mod}),
  ?assertEqual([{a, 11}, {b, 20}], lists:sort(eep_stats_agg:emit(S2))),
  S3 = eep_stats_agg:init(),
  S4 = eep_stats_agg:accumulate(S3, {a, 10, eep_stats_count}),
  S5 = eep_stats_agg:accumulate(S4, {b, 10, {eep_stats_sum, 50}}),
  ?assertEqual([{a, 1}, {b, 60}], lists:sort(eep_stats_agg:emit(S5))).

-endif.
