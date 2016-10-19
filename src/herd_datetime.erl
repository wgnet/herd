-module(herd_datetime).

-export([
    now/0, now_micro/0,
    datetime_from_db/1,
    timestamp_to_datetime/1,
    timestamp_to_db_datetime/1,
    datetime_to_timestamp/1,
    datetime_to_ISO/1,
    add_interval/2,
    subtract_interval/2
]).

-type(timestamp() :: integer()). % seconds, 1476882197
-type(timestamp_micro() :: float()). % int part in seconds, 1476882197.233323

%% PostgreSQL datetime format: 2014-12-20 17:38:56.475565
-type(db_datetime() :: {calendar:date(), {0..23, 0..59, float()}}).

-type(time_interval() :: {integer(), month | week | day | hour | minute | second}).

-define(DAY, 24 * 3600).


%%% module API

-spec now() -> timestamp().
now() ->
    erlang:system_time(seconds).


-spec now_micro() -> timestamp_micro().
now_micro() ->
    erlang:system_time(micro_seconds) / 1000000.


-spec datetime_from_db(db_datetime()) -> calendar:datetime().
datetime_from_db({Date, {Hour, Minute, Second}}) ->
    {Date, {Hour, Minute, trunc(Second)}}.


-spec timestamp_to_datetime(timestamp()) -> calendar:datetime().
timestamp_to_datetime(Timestamp) ->
    calendar:now_to_universal_time({Timestamp div 1000000, Timestamp rem 1000000, 0}).


-spec timestamp_to_db_datetime(timestamp_micro()) -> db_datetime().
timestamp_to_db_datetime(Timestamp) ->
    T = trunc(Timestamp),
    MicroSecs = Timestamp - T,
    {D, {H, M, S}} = calendar:now_to_universal_time({T div 1000000, T rem 1000000, 0}),
    {D, {H, M, S + MicroSecs}}.


-spec datetime_to_timestamp(calendar:datetime()) -> timestamp().
datetime_to_timestamp(DateTime) ->
    % 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.


-spec datetime_to_ISO(calendar:datetime()) -> string().
datetime_to_ISO({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
        io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b",
                      [Year, Month, Day, Hour, Minute, trunc(Second)])).


-spec add_interval(calendar:datetime(), time_interval()) -> calendar:datetime().
add_interval(Datetime, {M, month}) ->
    add_interval(Datetime, {M * 30 * ?DAY, second});

add_interval(Datetime, {W, week}) ->
    add_interval(Datetime, {W * 7 * ?DAY, second});

add_interval(Datetime, {D, day}) ->
    add_interval(Datetime, {D * ?DAY, second});

add_interval(Datetime, {H, hour}) ->
    add_interval(Datetime, {H * 3600, second});

add_interval(Datetime, {M, minute}) ->
    add_interval(Datetime, {M * 60, second});

add_interval(Datetime, {S, second}) ->
    T = datetime_to_timestamp(Datetime),
    timestamp_to_datetime(T + S).


-spec subtract_interval(calendar:datetime(), time_interval()) -> calendar:datetime().
subtract_interval(Datetime, {M, Type}) ->
    add_interval(Datetime, {-M, Type}).
