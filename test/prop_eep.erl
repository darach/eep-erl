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
%%  @author Michael Coles <michael.coles@gmail.com>
%%  @copyright (C) 2014, Darach Ennis, Michael Coles
%%  @doc
%%
%%  @end
%% -------------------------------------------------------------------
-module(prop_eep).

-include_lib("proper/include/proper.hrl").

-export([prop_avg_aggregate_accum/0]).
-export([prop_sum_aggregate_accum/0]).
-export([prop_monotonic_clock_count/0]).
-export([prop_monotonic_sliding_window/0]).
-export([prop_periodic_window/0]).
-export([prop_tumbling_window/0]).
-export([prop_sliding_window/0]).
-export([prop_sliding_time_window/0]).

-export([sliding_time_window/2]).
-export([expected_time_slider/2]).
-export([time_slider/2]).
-export([count_pushes/1]).
-export([stride/2]).

-export([go/0]).

-define(epsilon, 1.0e-15).

prop_avg_aggregate_accum() ->
    ?FORALL(Ints, non_empty(list(integer())),
            begin
                {RealSum, RealCount, MinInt, MaxInt, AggData} =
                    lists:foldl(
                      fun(N, {Sum, Count, Min, Max, State}) ->
                              {Sum+N, Count+1, min(Min, N), max(Max, N),
                               eep_stats_avg:accumulate(State, N)}
                      end, {0, 0, hd(Ints), hd(Ints), eep_stats_avg:init()}, Ints),
                AggAvg = eep_stats_avg:emit(AggData),
                RealAvg = RealSum / RealCount,
                %% ?epsilon fix: resize epsilon by the order of magnitude of
                %% the difference between min and max
                FudgeFactor = range_magnitude(MinInt, MaxInt),
                abs(AggAvg - RealAvg) < ?epsilon * FudgeFactor
            end).

range_magnitude(X, X) -> 1;
range_magnitude(Min, Max) ->
    Order = math:log10(abs(Max - Min)),
    math:pow(10, Order).

prop_sum_aggregate_accum() ->
    ?FORALL(Ints, non_empty(list(integer())),
            begin
                {RealSum, AggData} =
                    lists:foldl(
                      fun(N, {Sum, State}) ->
                              {Sum+N, eep_stats_sum:accumulate(State, N)}
                      end, {0, eep_stats_sum:init()}, Ints),
                    RealSum == eep_stats_sum:emit(AggData)
            end).

prop_monotonic_clock_count() ->
    ?FORALL({Interval, Events},
            {pos_integer(), list(tick)},
            begin
               Init = eep_clock_count:new(Interval),
               {Clock, Tocks} = lists:foldl(fun clock_handle/2,
                                            {Init, 0}, Events),
               ExpectedTime = length(Events) - (length(Events) rem Interval),
               eep_clock_count:at(Clock) == ExpectedTime
                    andalso Tocks == length(Events) div Interval
           end).

clock_handle(tick, {Clock, Tocks}) ->
    case eep_clock_count:tick(Clock) of
        {false, TickedClock} ->
            {TickedClock, Tocks};
        {true, TockingClock} ->
            case eep_clock_count:tock(TockingClock) of
                {true, Tocked} -> {Tocked, Tocks+1};
                {false, Tocked} -> {Tocked, Tocks}
            end
    end;
clock_handle(Event, _Clock) ->
    error({unhandled_clock_event, Event}).

prop_periodic_window() ->
    ?FORALL(
       {Agg, Clock, Interval, Events},
       {eep_stats_count, eep_clock_count, pos_integer(),
        list(oneof([tick, {push, 1}]))},
       begin
           Init = eep_window_periodic:new(Agg, Clock, fun(_) -> ok end, Interval),
           {_Final, Results} = lists:foldl(fun window_handle/2,
                                          {Init, []}, Events),
           {Emissions, _} =
               lists:partition(fun(emit) -> true; (_) -> false end, Results),
           {Ticks, _Datapoints} =
               lists:partition(fun(tick) -> true; (_) -> false end, Events),
           length(Emissions) == (length(Ticks)) div Interval
       end).

window_handle({push, _}=Push, {Window, Results}) ->
    {Act, Pushed} = eep_window_periodic:push(Window, Push),
    {Pushed, Results ++ [Act]};
window_handle(tick, {Window, Results}) ->
    {Act, Ticked} = eep_window_periodic:tick(Window),
    {Ticked, Results ++ [Act]}.

prop_monotonic_sliding_window() ->
    ?FORALL({Size, Integers}, {pos_integer(), non_empty(list(integer()))},
            begin
                Win = {eep_stats_sum:init(), Size, 1, [], none},
                %% TODO Refactor eep_window_sliding so we can use it directly here
                %% and not replicate its inner machinations below.
                WinFinal = lists:foldl(fun slide/2, Win, Integers),
                {_, Size, _, _, Emission} = WinFinal,
                if
                    % We expect no emission if the input list is less than our window size
                    Size > length(Integers) -> Emission == none;
                    Size =< length(Integers) ->
                        %% If Size is smaller than the number of input integers,
                        %% the accumulated value will only be the sum of the
                        %% last Size integers.
                        Include = lists:sublist(lists:reverse(Integers), 1, Size),
                        RealSum = lists:sum(Include),
                        Emission == RealSum
                end
            end).

%% NB This logic is copied directly from eep_window_sliding.erl
slide(Int, {Agg, Size, Count, Prior, LastEmission}) ->
    NewAgg = eep_stats_sum:accumulate(Agg, Int),
    if
        Count < Size ->
            NewPrior = Prior ++ [Int],
            {NewAgg, Size, Count+1, NewPrior, LastEmission};
        Count == Size ->
            NewPrior = Prior ++ [Int],
            {NewAgg, Size, Count+1, NewPrior, eep_stats_sum:emit(NewAgg)};
        true ->
            [Value | Tail] = Prior,
            NewAgg2 = eep_stats_sum:compensate(NewAgg, Value),
            NewPrior = Tail ++ [Int], {NewAgg2, Size, Count+1, NewPrior, eep_stats_sum:emit(NewAgg2)}
    end.

prop_tumbling_window() ->
    ?FORALL({Size, Integers}, {pos_integer(), non_empty(list(integer()))},
            begin
                W0 = eep_window:tumbling(events, Size, eep_stats_sum, []),
                {_Wn, As} = lists:foldl(fun winfold/2, {W0, []}, Integers),
                Emissions = [ eep_stats_sum:emit(A) || {emit,A} <- As],
                Noops = [noop || noop <- As],
                ExpEmissions = expected(Size, Integers),
                %% Number of 'emit' should be length(Integers) div Size
                length(Emissions) == length(Integers) div Size
                    %% The rest should have been noop
                    andalso length(Noops) == length(Integers) - length(Emissions)
                    %% The emitted values should match our expectation
                    %% (see expected/2 below)
                    andalso Emissions == ExpEmissions
            end).

prop_sliding_window() ->
    ?FORALL({Size, Integers}, {pos_integer(), non_empty(list(integer()))},
            begin
                W0 = eep_window:sliding(events, Size, eep_stats_count, []),
                {_Wn, As} = lists:foldl(fun winfold/2, {W0, []}, Integers),
                %% Expected: we emit for every event, except before we reach Size
                %% events;
                ExpEmissions = lists:duplicate(max(0, length(Integers) - Size + 1), Size),
                Emissions = [ eep_stats_count:emit(A) || {emit, A} <- As ],
                Noops = [ noop || noop <- As ],
                Emissions == ExpEmissions
                    andalso length(Noops) =< Size - 1
            end).

winfold(Ev, {Win, As}) ->
    {A, Win1} = eep_window:push(Ev, Win),
    {Win1, As++[A]}.

expected(WinSize, Integers) ->
    lists:reverse(expected(WinSize, Integers, [])).
expected(WinSize, Integers, SoFar)
  when WinSize > length(Integers) ->
    SoFar;
expected(WinSize, Integers, SoFar) ->
    Window = lists:sublist(Integers, 1, WinSize),
    expected(WinSize, lists:nthtail(WinSize, Integers), [lists:sum(Window) | SoFar]).

time_slider(push, {W, As}) ->
    {A, W1} = eep_window:push(1, W),
    {W1, [A | As]};
time_slider(tick, {W, As}) ->
    {A, W1} = eep_window:tick(W),
    {W1, [A | As]}.

prop_sliding_time_window() ->
    ?FORALL({Length, Events}, {pos_integer(), list(oneof([tick, push]))},
            %{2, lists:flatten([ [lists:duplicate(N, push), tick] || N <- [0, 1, 2, 4] ])},
            begin
                Actions = sliding_time_window(Length, Events),
                Exp = expected_time_slider(Length, Events),
                [ A || A <- Actions, A =/= noop ] == Exp
            end).

sliding_time_window(Length, Events) ->
    W0 = eep_window:sliding({clock, eep_clock_count, Length}, Length, eep_stats_count, []),
    {_Wn, As} = lists:foldl(fun time_slider/2, {W0, []}, Events),
    lists:reverse(As).

%% e.g.
%% Length = 2, Interval = 2
%% [ push, tick 1, push, push, tick 2, push, push, tick 3, tick 4  ]
%% [ noop, noop,   noop, noop, emit 3, noop, noop, noop,   emit 2 ]
expected_time_slider(WinSize, Events) ->
    stride(WinSize, count_pushes(Events)).

count_pushes(Events) ->
    count_pushes(Events, 0, []).
count_pushes([], _, Events) -> lists:reverse(Events);
count_pushes([tick | Raw], N, Events) ->
    count_pushes(Raw, 0, [ N | Events ]);
count_pushes([push | Raw], N, Events) ->
    count_pushes(Raw, N+1, Events).

stride(Size, Events) when Size > length(Events) -> [];
stride(Size, Events) ->
    Window = lists:sublist(Events, Size),
    Tail = lists:nthtail(Size, Events),
    [{emit, lists:sum(Window)} | stride(Size, Tail)].

go() ->
    case proper:counterexample(prop_sliding_time_window(), 1000) of
        true -> true;
        [{L, Es}] ->
            [{exp, expected_time_slider(L, Es)},
             {got, sliding_time_window(L, Es)}]
    end.
