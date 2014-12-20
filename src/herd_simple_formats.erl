-module(herd_simple_formats).

-export([get_date/1, get_time/1, get_datetime/1, get_ip/1]).


%%% module API

-spec get_date(string()) -> calendar:date() | error.
get_date(Str) ->
    case to_int_list(Str, "-") of
        [Y, M, D] when Y >= 0
                       andalso M >= 1 andalso M =< 12
                       andalso D >= 1 andalso D =< 31 ->
            {Y, M, D};
        _ -> error
    end.


-spec get_time(string()) -> calendar:time() | error.
get_time(Str) ->
    case to_int_list(Str, ":") of
        [H, M, S] when H >= 0 andalso H =< 24
                       andalso M >= 0 andalso M < 60
                       andalso S >= 0 andalso S < 60 ->
            {H, M, S};
        _ -> error
    end.


-spec get_datetime(string()) -> calendar:datetime() | error.
get_datetime(Str) ->
    case string:tokens(Str, " ") of
        [DStr, TStr | _] ->
            case {get_date(DStr), get_time(TStr)} of
                {error, _} -> error;
                {_, error} -> error;
                DT -> DT
            end;
        _ -> error
    end.


-spec get_ip(string()) -> {byte(), byte(), byte(), byte()} | error.
get_ip(Str) ->
    Ints = lists:filter(
             fun(N) when is_integer(N) andalso N >= 0 andalso N =< 255 -> true;
                (_) -> false
             end, to_int_list(Str, ".")),
    case Ints of
        [N1, N2, N3, N4] -> {N1, N2, N3, N4};
        _ -> error
    end.


%% inner functions

-spec to_int_list(string(), string()) -> [integer()].
to_int_list(Str, Delimeter) ->
    lists:filtermap(
      fun(S) ->
              case string:to_integer(S) of
                  {error, _} -> false;
                  {N, _} -> {true, N}
              end
      end,
      string:tokens(Str, Delimeter)).
