-module(herd_db).

-export([epgsql_res_to_json/1]).

-include("herd.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% module API

-spec epgsql_res_to_json({ok, [tuple()], [tuple()]}) -> [json_obj()].
epgsql_res_to_json({ok, Columns, Rows}) ->
    NameType = lists:map(fun({column, Name, Type, _, _, _}) -> {Name, Type} end, Columns),
    lists:map(fun(Row) -> epgsql_row_to_json(NameType, Row) end, Rows).


-spec epgsql_row_to_json([{binary(), atom()}], tuple()) -> json_obj().
epgsql_row_to_json(NameType, Row) ->
    KV = lists:map(fun(Index) ->
                           {Name, Type} = lists:nth(Index, NameType),
                           Value = epgsql_value_to_json(Type, element(Index, Row)),
                           {Name, Value}
                   end, lists:seq(1, length(NameType))),
    {KV}.


-spec epgsql_value_to_json(atom(), term()) -> json_value().
epgsql_value_to_json(timestamp, null) -> null;
epgsql_value_to_json(timestamp, Timestamp) ->
    herd_datetime:datetime_to_timestamp(
      herd_datetime:datetime_from_db(Timestamp));
epgsql_value_to_json(_Type, Value) -> Value.


epgsql_res_to_json_test() ->
    Data = {ok,
            [{column,<<"id">>,int8,8,-1,1},
             {column,<<"name">>,varchar,-1,36,1},
             {column,<<"registration_date">>,timestamp,8,-1,1},
             {column,<<"account_type">>,{unknown_oid,33521},4,-1,0},
             {column,<<"address">>,text,-1,-1,1},
             {column,<<"country">>,text,-1,-1,1}
            ],
            [{12,<<"Bob">>,{{2015,2,17},{16,10,31.277398}},<<"base">>,<<"Minsk, bla-bla-bla">>,<<"BY">>},
             {13,<<"Bill">>,{{2015,2,17},{16,11,1.211181}},<<"premium">>,<<"Amsterdam, bla-bla-bla">>,<<"NL">>}]},
    JSON = epgsql_res_to_json(Data),
    ?assertEqual([{[{<<"id">>,12},
                    {<<"name">>,<<"Bob">>},
                    {<<"registration_date">>,1424189431},
                    {<<"account_type">>,<<"base">>},
                    {<<"address">>,<<"Minsk, bla-bla-bla">>},
                    {<<"country">>,<<"BY">>}]},
                  {[{<<"id">>,13},
                    {<<"name">>,<<"Bill">>},
                    {<<"registration_date">>,1424189461},
                    {<<"account_type">>,<<"premium">>},
                    {<<"address">>,<<"Amsterdam, bla-bla-bla">>},
                    {<<"country">>,<<"NL">>}]}],
                 JSON),
    ok.
