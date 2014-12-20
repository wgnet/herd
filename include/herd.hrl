-type(timestamp() :: integer()).
-type(timestamp_micro() :: float()).

%% jiffy json format
-type(json_key() :: atom() | binary()).
-type(json_value() :: integer() | float() | binary() | json_obj() | [json_value()]).
-type(json_obj() :: {[{json_key(), json_value()}]}).


%% PostgreSQL returns datetime like this: 2014-12-20 17:38:56.475565+03
-type(db_datetime() :: {calendar:date(), {0..23, 0..59, float()}}).
