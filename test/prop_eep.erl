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
-include("eep_erl.hrl").

-export([prop_avg_aggregate_accum/0]).
-export([prop_sum_aggregate_accum/0]).
-export([prop_monotonic_clock_count/0]).
-export([prop_monotonic_sliding_window/0]).
-export([prop_periodic_window/0]).
-export([prop_tumbling_window/0]).
-export([prop_sliding_window/0]).
-export([prop_sliding_time_window/0]).
-export([prop_wall_clock/0]).

-export([sliding_time_window/2]).
-export([expected_time_slider/2]).
-export([time_slider/2]).
-export([compress_time_buckets/1]).
%-export([stride/2]).

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

prop_wall_clock() ->
    ?FORALL({Period, TickIntervals}, {500, list(integer(50, 100))},
            begin
                F = fun(I, {Cn, Cs, Ds, Ts}) ->
                        timer:sleep(I),
                        case eep_clock:tick(eep_clock_wall, Cn) of
                            {noop, Cn1} ->
                                Drift = eep_clock:at(Cn1) - (eep_clock:at(Cn) + I),
                                {Cn1, [Cn|Cs], [Drift|Ds], Ts};
                            {tock, Cn2} ->
                                Drift = eep_clock:at(Cn2) - (eep_clock:at(Cn) + I),
                                {Cn2, [Cn|Cs], [Drift|Ds], Ts+1}
                        end end,
                {_Cfinal, _Clocks, Drifts, Tocks} =
                    lists:foldl(F, {eep_clock_wall:new(Period), [], [], 0}, TickIntervals),
                ExpectedTocks = (lists:sum(Drifts) + lists:sum(TickIntervals)) div Period,
                ExpectedTocks == Tocks
            end).

prop_monotonic_clock_count() ->
    ?FORALL({Interval, Events},
            {pos_integer(), list(tick)},
            begin
               Init = eep_clock_count:new(Interval),
               {Clock, Tocks} = lists:foldl(fun clock_handle/2,
                                            {Init, 0}, Events),
               ExpectedTime = length(Events),
               eep_clock:at(Clock) == ExpectedTime
                    andalso Tocks == length(Events) div Interval
           end).

clock_handle(tick, {Clock, Tocks}) ->
    case eep_clock:tick(eep_clock_count, Clock) of
        {noop, TickedClock} ->
            {TickedClock, Tocks};
        {tock, TockingClock} ->
            {TockingClock, Tocks+1}
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
                WinFinal = lists:foldl(fun slide_fold/2, Win, Integers),
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

%% NB This logic is copied directly from what was eep_window_sliding.erl
slide_fold(Int, {Agg, Size, Count, Prior, LastEmission}) ->
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
                W0 = eep_window:tumbling(event, Size, eep_stats_sum, []),
                {_Wn, As} = lists:foldl(fun push_folder/2, {W0, []}, Integers),
                Emissions = [ eep_stats_sum:emit(A) || {emit,A} <- As],
                Noops = [noop || noop <- As],
                ExpEmissions = tumbling_expected(Size, Integers),
                %% Number of 'emit' should be length(Integers) div Size
                length(Emissions) == length(Integers) div Size
                    %% The rest should have been noop
                    andalso length(Noops) == length(Integers) - length(Emissions)
                    %% The emitted values should match our expectation
                    %% (see tumbling_expected/2 below)
                    andalso Emissions == ExpEmissions
            end).

tumbling_expected(WinSize, Integers) ->
    lists:reverse(tumbling_expected(WinSize, Integers, [])).
tumbling_expected(WinSize, Integers, SoFar)
  when WinSize > length(Integers) ->
    SoFar;
tumbling_expected(WinSize, Integers, SoFar) ->
    Window = lists:sublist(Integers, 1, WinSize),
    tumbling_expected(WinSize, lists:nthtail(WinSize, Integers), [lists:sum(Window) | SoFar]).

prop_sliding_window() ->
    ?FORALL({Size, Integers}, {pos_integer(), non_empty(list(integer()))},
            begin
                W0 = eep_window:sliding(event, Size, eep_stats_count, []),
                {_Wn, As} = lists:foldl(fun push_folder/2, {W0, []}, Integers),
                %% Expected: we emit for every event, except before we reach Size
                %% events;
                ExpEmissions = lists:duplicate(max(0, length(Integers) - Size + 1), Size),
                Emissions = [ eep_stats_count:emit(A) || {emit, A} <- As ],
                Noops = [ noop || noop <- As ],
                Emissions == ExpEmissions
                    andalso length(Noops) =< Size - 1
            end).

push_folder(Ev, {#eep_win{by=By, compensating=Com}=Win, As}) ->
    Tl = case By of event -> [tick]; time -> [] end,
    L = [{accumulate, Ev} | Tl],
    Post = case Com of true -> compensate; false -> reset end,
    case eep_window:decide(L, Win) of
        {{emit,_}=Em, Win1} ->
            {eep_window:Post(Win1), As++[Em]};
        {Other, Win1} ->
            {Win1, As++[Other]}
    end.

prop_sliding_time_window() ->
    ?FORALL({Length, Events}, {pos_integer(), list(oneof([tick, push]))},
            %{2, lists:flatten([ [lists:duplicate(N, push), tick] || N <- [0, 1, 2, 4] ])},
            begin
                Actions = sliding_time_window(Length, Events),
                Exp = expected_time_slider(Length, Events),
                [ A || A <- lists:flatten(Actions), A =/= noop ] == Exp
            end).

%% e.g.
%% Length = 2
%% [ push, push, tick, push, tick,                    tick,       push, tick, push, push, tick,        tick,                   tick ]
%% [ noop, noop, noop, noop, [{emit, 3}, {emit, 2}], [{emit, 1}], noop, noop, noop, noop, [{emit, 3}], [{emit, 2}, {emit, 1}], noop ]

expected_time_slider(Size, Events) ->
    create_emissions(Size, compress_time_buckets(Events), []).

compress_time_buckets(Raw) -> compress_time_buckets(Raw, 0, []).
compress_time_buckets([], _, Compd) -> lists:reverse(Compd);
compress_time_buckets([push | Raw], B, Bs) ->
    compress_time_buckets(Raw, B+1, Bs);
compress_time_buckets([tick | Raw], B, Bs) ->
    compress_time_buckets(Raw, 0, [B | Bs]).

create_emissions(S, Short, Emissions)
  when length(Short) < S -> Emissions;
create_emissions(_, [], Emissions) -> Emissions;
create_emissions(Size, Buckets, Emissions) ->
    [Bucket|Tail] = Buckets,
    WinScope = lists:sublist(Buckets, Size),
    WinTotal = lists:sum(WinScope),
    create_emissions(Size, Tail, Emissions++[ {emit, N} || N <- lists:seq(WinTotal, WinTotal-Bucket+1, -1) ]).

sliding_time_window(Length, Events) ->
    W0 = eep_window_sliding_time:new(eep_stats_count, eep_clock_count, fun(_) -> ok end, Length),
    {_Wn, As} = lists:foldl(fun time_slider/2, {W0, []}, Events),
    lists:reverse(As).

time_slider(push, {W, As}) ->
    {A, W1} = eep_window_sliding_time:push(W, 1),
    {W1, [A | As]};
time_slider(tick, {W, As}) ->
    {A, W1} = eep_window_sliding_time:tick(W),
    {W1, [A | As]}.
