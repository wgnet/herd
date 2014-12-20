-module(herd_string_test).

-include_lib("eunit/include/eunit.hrl").

%% eunit tests

fill_before_test() ->
    ?assertEqual("    abcdef", herd_string:fill_before("abcdef", 32, 10)),
    ?assertEqual("__abc", herd_string:fill_before("abc", $_, 5)),
    ?assertEqual("01", herd_string:fill_before("1", $0, 2)),
    ?assertEqual("000000000012345", herd_string:fill_before("12345", $0, 15)),
    ?assertEqual("abcdef", herd_string:fill_before("abcdef", 32, 5)),
    ?assertEqual("abcdef", herd_string:fill_before("abcdef", 32, 6)),
    ?assertEqual(" abcdef", herd_string:fill_before("abcdef", 32, 7)),
    ok.


split_test() ->
    ?assertEqual(["hello", "there"], herd_string:split("hello there", " ")),
    ?assertEqual(["he", "o there"], herd_string:split("hello there", "ll")),
    ?assertEqual(["h", "llo th", "r"], herd_string:split("hello there", "e")),
    ?assertEqual(["aa", "bbb", "ccc"], herd_string:split("aa___bbb___ccc", "___")),
    ?assertEqual(["aa", "bbb", "ccc"], herd_string:split("___aa___bbb___ccc", "___")),
    ?assertEqual(["aa", "bbb", "ccc"], herd_string:split("___aa___bbb___ccc___", "___")),
    ?assertEqual(["aa", "bbb", "ccc"], herd_string:split("aa___bbb___ccc___", "___")),
    ?assertEqual([], herd_string:split("", "")),
    ?assertEqual([], herd_string:split("", "aa")),
    ?assertEqual(["aa"], herd_string:split("aa", "")),
    ok.


replace_test() ->
    ?assertEqual("", herd_string:replace("", "old", "new")),
    ?assertEqual("aa|bb|cc", herd_string:replace("aa--bb--cc", "--", "|")),
    ?assertEqual("nice Cool", herd_string:replace("nice cool", "cool", "Cool")),
    ?assertEqual("AAbbcc", herd_string:replace("aabbcc", "aa", "AA")),
    ?assertEqual("aaBBcc", herd_string:replace("aabbcc", "bb", "BB")),
    ?assertEqual("aabbCC", herd_string:replace("aabbcc", "cc", "CC")),
    ?assertEqual("AAbbcc AAbbcc", herd_string:replace("aabbcc aabbcc", "aa", "AA")),
    ?assertEqual("aaBBcc aaBBcc", herd_string:replace("aabbcc aabbcc", "bb", "BB")),
    ?assertEqual("aabbCC aabbCC", herd_string:replace("aabbcc aabbcc", "cc", "CC")),
    ok.


escape_xml_test() ->
    ?assertEqual("hello &lt;b&gt;from &quot;Holliwood&quot;&lt;/b&gt;!",
                 herd_string:escape_xml("hello <b>from \"Holliwood\"</b>!")),
    ?assertEqual("one &amp; two", herd_string:escape_xml("one & two")),
    ok.


escape_json_test() ->
    ?assertEqual("hello \\\"there\\\" and\\n here\\n\\ragain\\t ok.",
                 herd_string:escape_json("hello \"there\" and\n here\n\ragain\t ok.")),
    ?assertEqual("\\\\ ok \\\\ ok",
                 herd_string:escape_json("\\ ok \\ ok")),
    ok.
