-module(herd_string).

-export([fill_before/3, split/2, replace/3,
         escape_xml/1, escape_json/1]).


%%% module API

-spec fill_before(string(), char(), integer()) -> string().
fill_before(Str, Char, ToLength) ->
    case length(Str) of
        Length when Length >= ToLength -> Str;
        _ -> fill_before([Char | Str], Char, ToLength)
    end.


-spec split(string(), string()) -> [string()].
split("", _) -> "";
split(Str, "") -> [Str];
split(Str, WithStr) -> split(Str, WithStr, [], []).


-spec split(string(), string(), [char()], [string()]) -> [string()].
split([], _, [], Acc2) -> lists:reverse(Acc2);
split([], _, Acc1, Acc2) -> lists:reverse([lists:reverse(Acc1) | Acc2]);
split(Str, WithStr, Acc1, Acc2) ->
    io:format("~p ~p ~p ~p", [Str, WithStr, Acc1, Acc2]),
    case lists:prefix(WithStr, Str) of
        true -> Str2 = lists:sublist(Str, length(WithStr) + 1, length(Str)),
                case Acc1 of
                    [] -> split(Str2, WithStr, [], Acc2);
                    _ -> split(Str2, WithStr, [], [lists:reverse(Acc1) | Acc2])
                end;
        false -> [Char | Str2] = Str,
                 split(Str2, WithStr, [Char | Acc1], Acc2)
    end.


-spec replace(string(), string(), string()) -> string().
replace([], _, _) -> "";
replace(Str, Old, New) ->
    case lists:prefix(Old, Str) of
        true -> Str2 = lists:sublist(Str, length(Old) + 1, length(Str)),
                New ++ replace(Str2, Old, New);
        false -> [Char | Str2] = Str,
                 [Char | replace(Str2, Old, New)]
    end.


-spec escape_xml(string()) -> string().
escape_xml(Str) ->
    lists:flatten(
      lists:map(fun($&) -> "&amp;";
                   ($>) -> "&gt;";
                   ($<) -> "&lt;";
                   ($") -> "&quot;";
                   (Char) -> Char
                end, Str)).


-spec escape_json(string()) -> string().
escape_json(Str) ->
    lists:flatten(
      lists:map(fun(92) -> "\\\\";
                   (34) -> "\\\"";
                   (10) -> "\\n";
                   (13) -> "\\r";
                   (9) -> "\\t";
                   (Char) -> Char
                end, Str)).
