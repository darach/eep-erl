%% -------------------------------------------------------------------
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
%% -------------------------------------------------------------------
%%  @author Darach Ennis <darach.ennis@gmail.com>
%%  @copyright (C) 2014, Darach Ennis
%%  @doc
%% 
%%  @end
%% -------------------------------------------------------------------
-module(eep_erl_SUITE).

-export([all/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([groups/0]).

-export([t_clock_wall/1]).
-export([t_clock_count/1]).
-export([t_win_tumbling_inline/1]).
-export([t_win_tumbling_process/1]).
-export([t_win_sliding_inline/1]).
-export([t_win_sliding_process/1]).
-export([t_win_periodic_inline/1]).
-export([t_win_periodic_process/1]).
-export([t_win_monotonic_inline/1]).
-export([t_win_monotonic_process/1]).
-export([t_seedable_aggregate/1]).
-export([t_avg_aggregate_accum/1]).
-export([t_sum_aggregate_accum/1]).
-export([t_monotonic_clock_count/1]).
-export([t_monotonic_sliding_window/1]).
-export([t_periodic_window/1]).
-export([t_tumbling_window/1]).
-export([t_sliding_window/1]).
-export([t_sliding_time_window/1]).

-include("eep_erl.hrl").
-include_lib("common_test/include/ct.hrl").

-define(proptest(TC), proper:quickcheck(TC)
                        orelse ct:fail({counterexample, proper:counterexample(TC)})).

all() ->
    [
        {group, clock},
        {group, win_tumbling},
        {group, win_sliding},
        {group, win_periodic},
        {group, win_monotonic},
        {group, aggregate},
        {group, props}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
        {clock, [], [
            t_clock_wall
            , t_clock_count
            ]},
        {win_tumbling, [sequence], [
            t_win_tumbling_inline,
            t_win_tumbling_process
            ]},
        {win_sliding, [sequence], [
            t_win_sliding_inline,
            t_win_sliding_process
            ]},
        {win_periodic, [sequence], [
            t_win_periodic_inline,
            t_win_periodic_process
            ]},
        {win_monotonic, [sequence], [
            t_win_monotonic_inline,
            t_win_monotonic_process
            ]},
        {aggregate, [], [
            t_seedable_aggregate
            ]},
        {props, [], [
            t_avg_aggregate_accum,
            t_sum_aggregate_accum,
            t_monotonic_clock_count,
            t_monotonic_sliding_window,
            t_periodic_window,
            t_tumbling_window,
            t_sliding_window,
            t_sliding_time_window
            ]}
    ].

init_per_suite(Config) ->
    Config.

t_clock_wall(_Config) ->
    M = eep_clock_wall,
    crock = M:name(),
    C0 = M:new(1),
    timer:sleep(1),
    T0 = M:ts(),
    true = (T0 - C0#eep_clock.at) >= 1,
    timer:sleep(1),
    {tock, C1} = eep_clock:tick(M, C0),
    true = (C1#eep_clock.at - T0) >= 1,
    T1 = C1#eep_clock.at,
    {tock,C2} = eep_clock:tick(M, C1),
    true = C2#eep_clock.mark =< T1.

t_clock_count(_Config) ->
    M = eep_clock_count,
    count = M:name(),
    C0 = M:new(2),
    0  = eep_clock:at(C0),
    {noop, C1} = eep_clock:tick(M, C0),
    1  = eep_clock:at(C1),
    {tock, C2} = eep_clock:tick(M, C1),
    {noop, C3} = eep_clock:tick(M, C2),
    {tock, C4} = eep_clock:tick(M, C3),
    {noop, C5} = eep_clock:tick(M, C4),
    {tock, C6} = eep_clock:tick(M, C5),
    6 = eep_clock:at(C6),
    6 = C6#eep_clock.mark.

t_win_tumbling_inline(_Config) ->
    W0  = eep_window_tumbling:new(eep_stats_count, fun(_Callback) -> boop end, 2),
    {_Fun, #eep_win{
              type=tumbling,
              by=event,
              compensating=false,
              aggmod=eep_stats_count,
              agg=0,
              clockmod=_,clock=_,
              count=1
             }} = W0,
    {noop,W1} = eep_window_tumbling:push(W0,foo),
    {emit,W2} = eep_window_tumbling:push(W1,bar),
    {noop,W3} = eep_window_tumbling:push(W2,baz),
    {emit,W4} = eep_window_tumbling:push(W3,bar),
    {_Fun, #eep_win{ agg=0, count=1 }} = W4,
    {noop,W5} = eep_window_tumbling:push(W4,foo),
    {_Fun, #eep_win{ agg=1, count=2 }} = W5,
    {emit,W6} = eep_window_tumbling:push(W5,bar),
    {noop,W7} = eep_window_tumbling:push(W6,foo),
    {emit,W8} = eep_window_tumbling:push(W7,bar),
    {_Fun, #eep_win{ agg=0, count=1 }} = W8,
    {noop,W9} = eep_window_tumbling:push(W8,foo),
    {emit,W10} = eep_window_tumbling:push(W9,bar),
    {_Fun, #eep_win{ agg=0, count=1 }} = W10,
    ok.

t_win_tumbling_process(_Config) ->
    Pid = eep_window_tumbling:start(eep_stats_count, 2),
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
        {debug, {_, #eep_win{
                       type=tumbling, by=event,
                       compensating=false,
                       size=2, count=1,
                       aggmod=eep_stats_count, agg=0
                      }}}=Debug0 ->
            Debug0;
        {debug, {_, #eep_win{}=Unex}} ->
            ct:fail({badmatch, Unex});
        Unexp -> ct:fail({unexpected, Unexp})
    end,
    Pid ! {push, baz},
    Pid ! {debug, self()},
    receive
        {debug, {_, #eep_win{agg=1, count=2}}}=Debug1 ->
            Debug1;
	    {debug, {_, #eep_win{}=Win}} ->
            ct:fail({badmatch, Win});
        Unexp2 -> ct:fail({unexpected, Unexp2})
    end,
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
        {debug, {_, #eep_win{agg=1, count=2}}}=Debug2 ->
            Debug2;
	    {debug, {_, #eep_win{}=Win2}} ->
            ct:fail({badmatch, Win2});
        Unexp3 ->
            ct:fail({unexpected, Unexp3})
    end,
    Pid ! {debug, self()},
    receive
        {debug, {_, #eep_win{agg=1, count=2}}}=Debug3 ->
            Debug3;
        {debug, {_, #eep_win{}=Win3}} ->
            ct:fail({badmatch, Win3});
        Unexp4 ->
            ct:fail({unexpected, Unexp4})
    end,
    Pid ! stop.

t_win_sliding_inline(_Config) ->
    W0 = eep_window_sliding:new(eep_stats_count, fun(_) -> boop end, 2),
    case W0 of
        {_, #eep_win{by=event, type=sliding,
                compensating=true, size=2,
                aggmod=eep_stats_count, agg=0}} -> ok;
        {_, #eep_win{}} -> ct:fail({badmatch, W0})
    end,
    {noop,W1} = eep_window_sliding:push(W0,foo),
    {emit,W2} = eep_window_sliding:push(W1,bar),
    case W2 of
        {_, #eep_win{agg=1, count=3}} -> ok;
        {_, #eep_win{}} -> ct:fail({badmatch, W2})
    end,
    {emit,W3} = eep_window_sliding:push(W2,baz),

    case W3 of
        {_, #eep_win{count=4, agg=1}} -> ok;
        {_, #eep_win{}} -> ct:fail({badmatch, W3})
    end,
    {emit,W4} = eep_window_sliding:push(W3,foo),
    {emit,W5} = eep_window_sliding:push(W4,bar),
    {emit,W6} = eep_window_sliding:push(W5,foo),
    {emit,W7} = eep_window_sliding:push(W6,bar),
    {emit,W8} = eep_window_sliding:push(W7,foo),
    {emit,W9} = eep_window_sliding:push(W8,bar),
    case W9 of
        {_, #eep_win{agg=1, count=10}} -> ok;
        {_, #eep_win{}} -> ct:fail({badmatch, W9})
    end.

t_win_sliding_process(_Config) ->
    Pid = eep_window_sliding:start(eep_stats_count, 2),
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
        {debug, {_, #eep_win{
                       by=event, type=sliding,
                       compensating=true, size=2,
                       aggmod=eep_stats_count, agg=1,
                       count=3
                      }}}=Debug0 -> Debug0;
        {debug, {_, #eep_win{}=Win0}} -> ct:fail({badmatch, Win0});
        Unexp0 -> ct:fail({unexpected, Unexp0})
    end,
    Pid ! {push, baz},
    Pid ! {debug, self()},
    receive
        {debug, {_, #eep_win{agg=1, count=4}}}=Debug1 -> Debug1;
        {debug, {_, #eep_win{}=Win1}} -> ct:fail({badmatch, Win1});
        Unexp1 -> ct:fail({unexpected, Unexp1})
    end,
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
        {debug, {_, #eep_win{agg=1, count=10}}}=Debug2 -> Debug2;
        {debug, {_, #eep_win{}=Win2}} -> ct:fail({badmatch, Win2});
        Unexp2 -> ct:fail({unexpected, Unexp2})
    end,
    Pid ! {debug, self()},
    receive
        {debug, {_, #eep_win{agg=1, count=10}}}=Debug3 -> Debug3;
        {debug, {_, #eep_win{}=Win3}} -> ct:fail({badmatch, Win3});
        Unexp3 -> ct:fail({unexpected, Unexp3})
    end,
    Pid ! stop.

t_win_periodic_inline(_Config) ->
    W0 = eep_window_periodic:new(eep_stats_count, eep_clock_count, fun(_) -> boop end, 1),
    {_Fun, E0} = W0,
    #eep_win{
       type=tumbling,
       by=time,
       compensating=false,
       clockmod=eep_clock_count,
       clock=#eep_clock{at=0, mark=0, interval=1},
       aggmod=eep_stats_count,
       agg=0,
       count=1 } = E0,
    {noop,W1} = eep_window_periodic:push(W0,foo),
    {noop,W2} = eep_window_periodic:push(W1,bar),
    E1 = E0#eep_win{agg=2, count=3, clock=#eep_clock{at=0, mark=0, interval=1}},
    {_Fun, E1} = W2,
    {emit,W3} = eep_window_periodic:tick(W2),
    E2 = E1#eep_win{agg=0, count=1, clock=#eep_clock{at=1, mark=1, interval=1}},
    {_Fun, E2} = W3,
    {noop,W4} = eep_window_periodic:push(W3,foo),
    {noop,W5} = eep_window_periodic:push(W4,bar),
    {noop,W6} = eep_window_periodic:push(W5,foo),
    {noop,W7} = eep_window_periodic:push(W6,bar),
    {noop,W8} = eep_window_periodic:push(W7,foo),
    {noop,W9} = eep_window_periodic:push(W8,bar),
    {emit,W10} = eep_window_periodic:tick(W9),
    E3 = E2#eep_win{agg=0, count=1, clock=#eep_clock{at=2, mark=2, interval=1}},
    {_Fun, E3} = W10,
    ok.

t_win_periodic_process(_Config) ->
  Pid = eep_window_periodic:start(eep_stats_count, eep_clock_count, 1),
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {debug, self()},
  E0 = #eep_win{
          type=tumbling,
          by=time,
          compensating=false,
          clockmod=eep_clock_count,
          clock=#eep_clock{at=0, mark=0, interval=1},
          aggmod=eep_stats_count,
          agg=2,
          size=1,
          count=3 },
  receive
    { debug, Debug0 } ->
          {_, E0} = Debug0
  end,
  Pid ! tick,
  Pid ! {debug, self()},
  E1 = E0#eep_win{agg=0, count=1, clock=#eep_clock{at=1, mark=1}},
  receive
    { debug, Debug1 } ->
          {_, E1} = Debug1
  end,
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {push, foo},
  Pid ! {push, bar},
  Pid ! {debug, self()},
  E2 = E1#eep_win{count=7, agg=6},
  receive
    { debug, Debug2 } ->
          {_, E2} = Debug2
  end,
  Pid ! tick,
  Pid ! {debug, self()},
  E3 = E2#eep_win{count=1, agg=0, clock=#eep_clock{at=2, mark=2}},
  receive
    { debug, Debug3 } ->
          {_, E3} = Debug3
  end,
  Pid ! stop.

t_win_monotonic_inline(_Config) ->
    W0 = eep_window_monotonic:new(eep_stats_count, eep_clock_count, fun(_) -> boop end, 0),
    {noop,W1} = eep_window_monotonic:push(W0,foo),
    {noop,W2} = eep_window_monotonic:push(W1,bar),
    {_, #eep_win{count=3}} = W2,
    %{state,undefined,eep_stats_count,[],eep_clock_count,{eep_clock,3,0,0},2,_} = W2,
    {emit,W3} = eep_window_monotonic:tick(W2),
    {_, #eep_win{count=1}} = W3,
    %{state,undefined,eep_stats_count,[],eep_clock_count,{eep_clock,_,_,0},0,_} = W3,
    {noop,W4} = eep_window_monotonic:push(W3,foo),
    {noop,W5} = eep_window_monotonic:push(W4,bar),
    {noop,W6} = eep_window_monotonic:push(W5,foo),
    {noop,W7} = eep_window_monotonic:push(W6,bar),
    {noop,W8} = eep_window_monotonic:push(W7,foo),
    {noop,W9} = eep_window_monotonic:push(W8,bar),
    {emit,W10} = eep_window_monotonic:tick(W9),
    {_, #eep_win{count=1}} = W10,
    %{state,undefined,eep_stats_count,[],eep_clock_count,{eep_clock,_,_,0},0,_} = W10,
    ok.

t_win_monotonic_process(_config) ->
    Pid = eep_window_monotonic:start(eep_stats_count, eep_clock_count, 0),
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
        { debug, Debug0 } ->
            {_, #eep_win{count=3}} = Debug0
    end,
    Pid ! tick,
    Pid ! {debug, self()},
    receive
        { debug, Debug1 } ->
            {_, #eep_win{count=1}} = Debug1
    end,
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {push, foo},
    Pid ! {push, bar},
    Pid ! {debug, self()},
    receive
        { debug, Debug2 } ->
            {_, #eep_win{count=7}} = Debug2
    end,
    Pid ! tick,
    Pid ! {debug, self()},
    receive
        { debug, Debug3 } ->
            {_, #eep_win{count=1}} = Debug3
    end,
    Pid ! stop.

t_seedable_aggregate(_Config) ->
    [seed] = seedable_aggregate:init(),
    [meep] = seedable_aggregate:init([meep]),
    [meep,moop] = seedable_aggregate:accumulate([meep],moop),
    [moop,morp] = seedable_aggregate:compensate([meep,moop],morp),
    [meep,moop] = seedable_aggregate:emit([meep,moop]),
    ok.

t_avg_aggregate_accum(_) ->
    ?proptest(prop_eep:prop_avg_aggregate_accum()).

t_sum_aggregate_accum(_) ->
    ?proptest(prop_eep:prop_sum_aggregate_accum()).

t_monotonic_clock_count(_) ->
    ?proptest(prop_eep:prop_monotonic_clock_count()).

t_periodic_window(_) ->
    ?proptest(prop_eep:prop_periodic_window()).

t_tumbling_window(_) ->
    ?proptest(prop_eep:prop_tumbling_window()).

t_sliding_window(_) ->
    ?proptest(prop_eep:prop_sliding_window()).

t_monotonic_sliding_window(_) ->
    ?proptest(prop_eep:prop_monotonic_sliding_window()).

t_sliding_time_window(_) ->
    ?proptest(prop_eep:prop_sliding_time_window()).
