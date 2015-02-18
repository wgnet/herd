-type(timestamp() :: integer()).
-type(timestamp_micro() :: float()).

%% jiffy json format
-type(json_key() :: atom() | binary()).
-type(json_value() :: atom() | integer() | float() | binary() | json_obj() | [json_value()]).
-type(json_obj() :: {[{json_key(), json_value()}]}).

%% db types (PostgreSQL with epgsql driver)
-type(db_name() :: binary()).
-type(db_type() :: atom()).
-type(db_value() :: term()).
-type(db_row() :: tuple()).
-type(db_column() :: {column, db_name(), db_type(), term(), term(), term()}).
-type(db_select() :: {ok, [db_column()], [db_row()]}).
-type(db_datetime() :: {calendar:date(), {0..23, 0..59, float()}}). % PostgreSQL datetime format: 2014-12-20 17:38:56.475565+03
