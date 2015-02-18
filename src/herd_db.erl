-module(herd_db).

-export([epgsql_res_to_json/1]).

-include("herd.hrl").


%%% module API

-spec epgsql_res_to_json(db_select()) -> [json_obj()].
epgsql_res_to_json({ok, Columns, Rows}) ->
    lists:map(fun(Row) -> epgsql_row_to_json(Columns, Row) end, Rows).


%%% inner functions

-spec epgsql_row_to_json([db_column()], db_row()) -> json_obj().
epgsql_row_to_json(Columns, Row) ->
    KV = lists:map(
           fun(Index) ->
                   {column, Name, Type, _, _, _} = lists:nth(Index, Columns),
                   Value = epgsql_value_to_json(Type, element(Index, Row)),
                   {Name, Value}
           end, lists:seq(1, length(Columns))),
    {KV}.


-spec epgsql_value_to_json(db_type(), db_value()) -> json_value().
epgsql_value_to_json(_Type, Value) when is_atom(Value);
                                        is_integer(Value);
                                        is_float(Value);
                                        is_binary(Value)
                                        -> Value;
epgsql_value_to_json(time, {H, M, S}) -> H * 3600 + M * 60 + trunc(S);
epgsql_value_to_json(date, Date) -> epgsql_value_to_json(timestamp, {Date, {0, 0, 0.0}});
epgsql_value_to_json(timestamp, Timestamp) -> epgsql_value_to_json(timestamptz, Timestamp);
epgsql_value_to_json(timestamptz, Timestamp) ->
    herd_datetime:datetime_to_timestamp(
      herd_datetime:datetime_from_db(Timestamp));
epgsql_value_to_json(Type, Value) -> throw({type_not_supported, Type, Value}).
