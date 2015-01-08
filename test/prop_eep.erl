-module(prop_eep).

-include_lib("proper/include/proper.hrl").

-export([prop_avg_aggregate_accum/0]).
-export([prop_monotonic_clock_count/0]).
-export([prop_periodic_window/0]).

-define(epsilon, 1.0e-14).

prop_avg_aggregate_accum() ->
    ?FORALL(Ints, non_empty(list(integer())),
            begin
                {RealSum, RealCount, AggData} = 
                    lists:foldl(
                      fun(N, {Sum, Count, State}) ->
                              {Sum+N, Count+1, eep_stats_avg:accumulate(State, N)}
                      end, {0, 0, eep_stats_avg:init()}, Ints),
                AggAvg = eep_stats_avg:emit(AggData),
                RealAvg = RealSum / RealCount,
                (AggAvg - RealAvg) < ?epsilon
            end).

prop_monotonic_clock_count() ->
    ?FORALL({Interval, Events},
            {pos_integer(), list(tick)},
            begin 
               Init = eep_clock_count:new(Interval),
               {Clock, Tocks} = lists:foldl(fun clock_handle/2,
                                            {Init, 0}, Events),
               length(Events) == eep_clock_count:at(Clock)
                    andalso Tocks == length(Events) div Interval
           end).

clock_handle(tick, {Clock, Tocks}) ->
    case eep_clock_count:tick(Clock) of
        {false, TickedClock} ->
            {TickedClock, Tocks};
        {true, TockingClock} ->
            case eep_clock_count:tock(TockingClock, undefined) of
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
