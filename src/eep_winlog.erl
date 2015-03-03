-module(eep_winlog).

-export([new/0]).
-export([append/2]).
-export([tick/2]).
-export([expire/2]).
-export([current/1]).

-define(tick(T), {?MODULE, tick, T}).

-type log() :: list(any()).

new() -> [].

append(Event, Log) -> Log ++ [Event].

tick(Time, Log) -> Log ++ [?tick(Time)].

-spec expire(TStamp :: any(), Old :: log()) ->
    {Expired :: list(any()), New :: log()}.
expire(Limit, Log) -> %% TODO This leaves the expired tick (but nothing before it)
    {Expired, Remaining} = lists:splitwith(fun(?tick(T)) when T > Limit -> false;
                                              (_) -> true end, Log),
    {current(Expired), Remaining}.

-spec current(log()) -> log().
current(Log) ->
    lists:filter(fun(?tick(_)) -> false;
                    (_) -> true end, Log).
