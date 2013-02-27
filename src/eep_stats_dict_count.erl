%%% @author Morton Swimmer <morton at swimmer dot org>
%%% @copyright (C) 2013, Morton Swimmer
%%% @doc
%%% An aggregation module for named entities that counts on a per-entity basis.
%%% Internally, it uses an Erlang dictionary so this can use a lot
%%% of memory if you let it run long and you have a large diversity 
%%% of entities.
%%% @end
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to the
%%% following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% Created : 26 Feb 2013 by Morton Swimmer <morton at swimmer dot org>

-module(eep_stats_dict_count).

-include_lib("eep_erl.hrl").

-behaviour(eep_aggregate).

%% aggregate behaviour.
-export([init/0]).
-export([accumulate/2]).
-export([compensate/2]).
-export([emit/1]).

-record(state, {acc_dict}).

%%--------------------------------------------------------------------
%% @doc
%% Initialize the dict counter with a new, fresh, dictionary.
%% @end
%%--------------------------------------------------------------------
-spec init() -> record().

init() ->
    #state{acc_dict = dict:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Accumulate an event with key X
%% @end
%%--------------------------------------------------------------------
-spec accumulate(State :: record(), X :: any()) -> record().

accumulate(#state{acc_dict = AccDict} = State, X) ->
    State#state{acc_dict = dict:update_counter(X, 1, AccDict)}.

%%--------------------------------------------------------------------
%% @doc
%% Compensate for an event with key X
%% @end
%%--------------------------------------------------------------------
-spec compensate(State :: record(), X :: any()) -> record().

compensate(#state{acc_dict = AccDict} = State, X) ->
    State#state{acc_dict = dict:update_counter(X, -1, AccDict)}.

%%--------------------------------------------------------------------
%% @doc
%% Prepare the results for emit and return it. We just return the dictionary.
%% @end
%%--------------------------------------------------------------------
-spec emit(record()) -> dict().

emit(#state{acc_dict = AccDict}) ->
    AccDict.

-ifdef(TEST).

basic_test() ->
    R0 = init(),
    #state{acc_dict = D0} = R0,
    ?assertEqual(0, dict:size(D0)),
    R1 = accumulate(R0, a),
    #state{acc_dict = D1} = R1,
    ?assertEqual(1, dict:size(D1)),

    R2 = accumulate(R1, a),
    #state{acc_dict = D2} = R2,
    ?assertEqual(1, dict:size(D2)),
    ?assertEqual(2, dict:fetch(a, D2)),

    R3 = accumulate(R2, b),
    #state{acc_dict = D3} = R3,
    ?assertEqual(2, dict:size(D3)),
    ?assertEqual(2, dict:fetch(a, D3)),
    ?assertEqual(1, dict:fetch(b, D3)),

    R4 = compensate(R3, a),
    #state{acc_dict = D4} = R4,
    ?assertEqual(2, dict:size(D4)),
    ?assertEqual(1, dict:fetch(a, D4)),
    ?assertEqual(1, dict:fetch(b, D4)),

    ?assertEqual(2, dict:size(emit(R4))).

-endif.
