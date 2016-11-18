-module(herd_datetime_tests).
-include_lib("eunit/include/eunit.hrl").

-spec datetime_from_db_test() -> ok.
datetime_from_db_test() ->
    ?assertEqual({{2016, 11, 18}, {13, 55, 12}},
        herd_datetime:datetime_from_db({{2016, 11, 18}, {13, 55, 12.632}})),
    ok.


-spec timestamp_to_datetime_test() -> ok.
timestamp_to_datetime_test() ->
    ?assertEqual({{2016,11,18},{10,57,44}},
        herd_datetime:timestamp_to_datetime(1479466664)),
    ok.

-spec timestamp_to_db_datetime_test() -> ok.
timestamp_to_db_datetime_test() ->
    {{2016,11,18},{10,57,S}} = herd_datetime:timestamp_to_db_datetime(1479466664.123456),
    ?assert(abs(S - 44.123456) < 0.0000001),
    ok.


-spec datetime_to_ISO_test() -> ok.
datetime_to_ISO_test() ->
    ?assertEqual("2016-11-18T13:55:12",
        herd_datetime:datetime_to_ISO({{2016, 11, 18}, {13, 55, 12}})),
    ok.


-spec add_interval_test() -> ok.
add_interval_test() ->
    DT = {{2016, 11, 18}, {13, 55, 12}},
    ?assertEqual({{2016, 11, 18}, {13, 55, 14}},
        herd_datetime:add_interval(DT, {2, second})),
    ?assertEqual({{2016, 11, 18}, {14, 2, 12}},
        herd_datetime:add_interval(DT, {7, minute})),
    ?assertEqual({{2016, 11, 18}, {15, 55, 12}},
        herd_datetime:add_interval(DT, {2, hour})),
    ?assertEqual({{2016, 11, 19}, {3, 55, 12}},
        herd_datetime:add_interval(DT, {14, hour})),
    ?assertEqual({{2016, 11, 20}, {13, 55, 12}},
        herd_datetime:add_interval(DT, {2, day})),
    ?assertEqual({{2016, 12, 8}, {13, 55, 12}},
        herd_datetime:add_interval(DT, {20, day})),
    ?assertEqual({{2016, 11, 25}, {13, 55, 12}},
        herd_datetime:add_interval(DT, {1, week})),
    ?assertEqual({{2016, 12, 2}, {13, 55, 12}},
        herd_datetime:add_interval(DT, {2, week})),
    ok.


-spec subtract_interval_test() -> ok.
subtract_interval_test() ->
    DT = {{2016, 11, 18}, {13, 55, 12}},
    ?assertEqual({{2016, 11, 18}, {13, 55, 10}},
        herd_datetime:subtract_interval(DT, {2, second})),
    ?assertEqual({{2016, 11, 18}, {13, 48, 12}},
        herd_datetime:subtract_interval(DT, {7, minute})),
    ?assertEqual({{2016, 11, 18}, {11, 55, 12}},
        herd_datetime:subtract_interval(DT, {2, hour})),
    ?assertEqual({{2016, 11, 17}, {23, 55, 12}},
        herd_datetime:subtract_interval(DT, {14, hour})),
    ?assertEqual({{2016, 11, 16}, {13, 55, 12}},
        herd_datetime:subtract_interval(DT, {2, day})),
    ?assertEqual({{2016, 10, 29}, {13, 55, 12}},
        herd_datetime:subtract_interval(DT, {20, day})),
    ?assertEqual({{2016, 11, 11}, {13, 55, 12}},
        herd_datetime:subtract_interval(DT, {1, week})),
    ?assertEqual({{2016, 11, 4}, {13, 55, 12}},
        herd_datetime:subtract_interval(DT, {2, week})),
    ok.