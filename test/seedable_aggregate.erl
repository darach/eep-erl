-module(seedable_aggregate).

-behaviour(eep_aggregate).

-export([init/0]).
-export([init/1]).
-export([accumulate/2]).
-export([compensate/2]).
-export([emit/1]).

init() ->
    init([seed]).

init(Args) ->
    State=Args, State.

accumulate(State,Event) ->
    State ++ [Event].

compensate(State,Event) ->
    erlang:tl(State) ++ [Event].

emit(State) ->
    State.
