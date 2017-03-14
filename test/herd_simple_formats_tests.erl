-module(herd_simple_formats_tests).

-include_lib("eunit/include/eunit.hrl").

%% eunit tests

get_date_test() ->
    ?assertEqual({2014, 5, 20}, herd_simple_formats:get_date("2014-05-20")),
    ?assertEqual({2014, 5, 20}, herd_simple_formats:get_date("2014-5-20")),
    ?assertEqual({1970, 1, 1}, herd_simple_formats:get_date("1970-1-1")),
    ?assertEqual({1970, 1, 1}, herd_simple_formats:get_date("1970-01-01")),
    ?assertEqual({1970, 12, 31}, herd_simple_formats:get_date("1970-12-31")),
    ?assertEqual({1970, 1, 1}, herd_simple_formats:get_date("1970-1-1-bla")),
    ?assertEqual(error, herd_simple_formats:get_date("2000-13-31")),
    ?assertEqual(error, herd_simple_formats:get_date("2000-10-32")),
    ?assertEqual(error, herd_simple_formats:get_date("bla-bla-bla")),
    ?assertEqual(error, herd_simple_formats:get_date("1970-10-bla")),
    ok.


get_time_test() ->
    ?assertEqual({23, 15, 34}, herd_simple_formats:get_time("23:15:34")),
    ?assertEqual({12, 0, 11}, herd_simple_formats:get_time("12:00:11")),
    ?assertEqual({0, 0, 0}, herd_simple_formats:get_time("00:00:00")),
    ?assertEqual({1, 0, 0}, herd_simple_formats:get_time("01:00:00")),
    ?assertEqual({0, 1, 0}, herd_simple_formats:get_time("00:01:00")),
    ?assertEqual({0, 0, 1}, herd_simple_formats:get_time("00:00:01")),
    ?assertEqual({1, 1, 1}, herd_simple_formats:get_time("01:01:01")),
    ?assertEqual({23, 59, 59}, herd_simple_formats:get_time("23:59:59")),
    ?assertEqual(error, herd_simple_formats:get_time("01:01:01:01")),
    ?assertEqual(error, herd_simple_formats:get_time("01:01:ab")),
    ?assertEqual(error, herd_simple_formats:get_time("01:01")),
    ?assertEqual(error, herd_simple_formats:get_time("25:10:10")),
    ?assertEqual(error, herd_simple_formats:get_time("24:60:10")),
    ?assertEqual(error, herd_simple_formats:get_time("24:30:60")),
    ok.


get_datetime_test() ->
    ?assertEqual(error, herd_simple_formats:get_datetime("")),
    ?assertEqual(error, herd_simple_formats:get_datetime("bla-bla-bla")),
    ?assertEqual(error, herd_simple_formats:get_datetime("bla-bla-bla bla:bla:bla")),
    ?assertEqual(error, herd_simple_formats:get_datetime("2014-05-20")),
    ?assertEqual(error, herd_simple_formats:get_datetime("2014-05-20 bla-bla-bla")),
    ?assertEqual(error, herd_simple_formats:get_datetime("2014-05-20 bla bla bla")),
    ?assertEqual(error, herd_simple_formats:get_datetime("blablabla 23:15:34")),
    ?assertEqual({{2014, 5, 20}, {23, 15, 34}},
                 herd_simple_formats:get_datetime("2014-05-20 23:15:34")),
    ?assertEqual({{2014, 5, 20}, {12, 0, 11}},
                 herd_simple_formats:get_datetime("2014-5-20 12:00:11")),
    ?assertEqual({{1970, 1, 1}, {0, 0, 0}},
                 herd_simple_formats:get_datetime("1970-1-1 00:00:00")),
    ?assertEqual({{1970, 1, 1}, {23, 5, 9}},
                 herd_simple_formats:get_datetime("1970-01-01 23:05:09")),
    ?assertEqual({{1970, 12, 31}, {23, 59, 59}},
                 herd_simple_formats:get_datetime("1970-12-31 23:59:59")),
    ?assertEqual({{2014, 9, 19}, {12, 59, 30}},
                 herd_simple_formats:get_datetime("2014-09-19 12:59:30 UTC")),
    ok.


get_ip_test() ->
    ?assertEqual({0, 0, 0, 0}, herd_simple_formats:get_ip("0.0.0.0")),
    ?assertEqual({1, 2, 3, 4}, herd_simple_formats:get_ip("1.2.3.4")),
    ?assertEqual({127, 0, 0, 1}, herd_simple_formats:get_ip("127.0.0.1")),
    ?assertEqual({255, 255, 255, 255}, herd_simple_formats:get_ip("255.255.255.255")),
    ?assertEqual({10, 1, 0, 20}, herd_simple_formats:get_ip("10.1.0.20")),
    ?assertEqual({173, 194, 112, 238}, herd_simple_formats:get_ip("173.194.112.238")),
    ?assertEqual({93, 158, 134, 3}, herd_simple_formats:get_ip("93.158.134.3")),
    ?assertEqual(error, herd_simple_formats:get_ip("255.255.255.256")),
    ?assertEqual(error, herd_simple_formats:get_ip("255.255.255.500")),
    ?assertEqual(error, herd_simple_formats:get_ip("1.2.3")),
    ?assertEqual(error, herd_simple_formats:get_ip("-1.2.3.4")),
    ?assertEqual(error, herd_simple_formats:get_ip("1.2.3.4.5")),
    ?assertEqual(error, herd_simple_formats:get_ip("a.b.c.d")),
    ?assertEqual(error, herd_simple_formats:get_ip("hellothere")),
    ?assertEqual(error, herd_simple_formats:get_ip("127.hello.there.again")),
    ok.
