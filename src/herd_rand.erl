-module(herd_rand).

-export([init_crypto/0, str/1, uuid/0, md5hex/1, hex/0]).


%%% module API

%% sets random seed for current process
-spec init_crypto() -> ok.
init_crypto() ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {A,B,C}),
    ok.


%% generates random string which consists of 0-9A-Za-z chars
-spec str(integer()) -> string().
str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [47 + rand:uniform(109) || _ <- lists:seq(1, Length)]).


%% generates UUIDs like "85b3a1cf-d003-4548-a16d-c7739c18f519"
-spec uuid() -> string().
uuid() ->
    <<R1:48, _:4, R2:12, _:2, R3:62>> = crypto:strong_rand_bytes(16),
    Rand =
        <<R1:48,
          0:1, 1:1, 0:1, 0:1, % version
          R2:12,
          1:1, 0:1,           % RFC 4122
          R3:62>>,
    <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = Rand,
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
                                [TL, TM, THV, CSR, CSL, N])).


%% gets md5 hash from string
-spec(md5hex(string()) -> string()).
md5hex(Str) ->
    <<Hash:128/integer>> = erlang:md5(Str),
    string:to_lower(integer_to_list(Hash, 16)).


%% generates random md5 hash
-spec hex() -> string().
hex() -> md5hex(integer_to_list(erlang:phash2({os:system_time(micro_seconds), make_ref()}))).

