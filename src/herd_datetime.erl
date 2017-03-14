-module(herd_datetime).

-export([now/0, now_micro/0,
         datetime_from_db/1,
         timestamp_to_datetime/1, datetime_to_timestamp/1,
         datetime_to_ISO/1]).

-include("herd.hrl").


%%% module API

-spec now() -> timestamp().
now() ->
    os:system_time(seconds).


-spec now_micro() -> timestamp_micro().
now_micro() ->
    os:system_time(micro_seconds) * 1.0e-6.


-spec datetime_from_db(db_datetime()) -> calendar:datetime().
datetime_from_db({Date, {Hour, Minute, Second}}) ->
    {Date, {Hour, Minute, trunc(Second)}}.


-spec timestamp_to_datetime(timestamp() | timestamp_micro()) -> calendar:datetime().
timestamp_to_datetime(Timestamp) ->
    calendar:now_to_universal_time({Timestamp div 1000000, Timestamp rem 1000000, 0}).


-spec datetime_to_timestamp(calendar:datetime()) -> timestamp().
datetime_to_timestamp(DateTime) ->
    % 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.


-spec datetime_to_ISO(calendar:datetime()) -> string().
datetime_to_ISO({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
        io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b",
                      [Year, Month, Day, Hour, Minute, trunc(Second)])).
