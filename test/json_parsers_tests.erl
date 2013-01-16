%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2012, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2012 by <eduard.sergeev@gmail.com>
%%%-------------------------------------------------------------------
-module(json_parsers_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("meta/include/meta.hrl").
-include("../src/json_parsers.hrl").



ws_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     skip_many(
                       whitespace()),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {ok, <<"">>}}, P(<<"">>)),
     ?_assertMatch({ok, {ok, <<"1">>}}, P(<<"   1">>)),
     ?_assertMatch({ok, {ok, <<"1 ">>}}, P(<<"\n\t \r 1 ">>))].

boolean_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     boolean(),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {true, <<>>}}, P(<<"true">>)),
     ?_assertMatch({ok, {false, <<>>}}, P(<<"false">>)),
     ?_assertMatch({error, _}, P(<<"tru">>)),
     ?_assertMatch({error, _}, P(<<"tru1">>)),
     ?_assertMatch({ok, {true, <<"1">>}}, P(<<"true1">>)),
     ?_assertMatch({ok, {false, <<"1">>}}, P(<<"false1">>))].

integer_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     integer(),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok,{42, <<>>}}, P(<<"42">>)),
     ?_assertMatch({ok,{42, <<"a">>}}, P(<<"42a">>)),
     ?_assertMatch({ok,{42, <<".">>}}, P(<<"42.">>)),
     ?_assertMatch({ok,{-42, <<>>}}, P(<<"-42">>)),
     ?_assertMatch({ok,{0, <<>>}}, P(<<"0">>)),
     ?_assertMatch({ok,{0, <<>>}}, P(<<"-0">>)),
     ?_assertMatch({ok,{0, <<"1">>}}, P(<<"01">>)),
     ?_assertMatch({error, _}, P(<<"a">>)),
     ?_assertMatch({error, _}, P(<<"">>))].

float_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     float(),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok,{42.0, <<>>}},
                   P(<<"42">>)),
     ?_assertMatch({ok,{42.0, <<"a">>}},
                   P(<<"42a">>)),
     ?_assertMatch({ok,{42.0, <<>>}},
                   P(<<"42.0">>)),
     ?_assertMatch({ok,{-42.0, <<>>}},
                   P(<<"-42">>)),
     ?_assertMatch({ok,{-42.0, <<"a">>}},
                   P(<<"-42a">>)),
     ?_assertMatch({ok,{-42.0, <<>>}},
                   P(<<"-42.0">>)),
     ?_assertMatch({ok,{42.0, <<".">>}},
                   P(<<"42.">>)),
     ?_assertMatch({ok,{0.0, <<>>}},
                   P(<<"0">>)),
     ?_assertMatch({ok,{0.0, <<>>}},
                   P(<<"-0">>)),
     ?_assertMatch({ok,{0.0, <<"1">>}},
                   P(<<"01">>)),
     ?_assertMatch({ok,{42.0E35, <<>>}},
                   P(<<"42.0E35">>)),
     ?_assertMatch({ok,{-42.0E35, <<>>}},
                   P(<<"-42.0E35">>)),
     ?_assertMatch({ok,{42.0E-35, <<>>}},
                   P(<<"42.0E-35">>)),
     ?_assertMatch({ok,{-42.0E-35, <<>>}},
                   P(<<"-42.0E-35">>)),
     ?_assertMatch({ok,{-42.0E35, <<>>}},
                   P(<<"-42.0E35">>)),
     ?_assertMatch({ok,{42.1234e12, <<>>}},
                   P(<<"42.1234E12">>)),
     ?_assertMatch({ok,{42.0E35, <<>>}},
                   P(<<"42E35">>)),
     ?_assertMatch({ok,{42.0, <<>>}},
                   P(<<"42E0">>)),
     ?_assertMatch({ok,{420.0, <<>>}},
                   P(<<"42E1">>)),
     ?_assertMatch({error, _},
                   P(<<"a">>)),
     ?_assertMatch({error, _},
                   P(<<"">>)),
     ?_assertMatch({error, _},
                   P(<<".E0">>))].

string_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     string(),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok,{<<"">>, <<>>}},
                   P(<<"\"\"">>)),
     ?_assertMatch({ok,{<<"Simple">>, <<>>}},
                   P(<<"\"Simple\"">>)),
     ?_assertMatch({ok,{<<"\" \\ / \b \f \n \r \t ", 1024/utf8>>, <<>>}},
                   P(<<"\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t \\u0400\"">>)),
     ?_assertMatch({ok,{<<" d ", 8211/utf8, " ", 8220/utf8, "Time Slowing Down", 8221/utf8>>, <<>>}},
                   P(<<"\" d \\u2013 \\u201cTime Slowing Down\\u201d\"">>)),
     ?_assertMatch({error, _},
                   P(<<"\"">>)),
     ?_assertMatch({error, _},
                   P(<<"abc">>))].
    

array_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     array(nullable(float())),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok,{[], <<>>}},
                   P(<<"[]">>)),
     ?_assertMatch({ok,{[42.0], <<>>}},
                   P(<<"[42]">>)),
     ?_assertMatch({ok,{[1.0 ,2.0 ,3.0 , 4.0], <<>>}},
                   P(<<"[1,2, 3  \n,4\t]">>)),
     ?_assertMatch({ok,{[1.0, undefined, 2.0], <<>>}},
                   P(<<"[1.0, null, 2.0]">>)),
     ?_assertMatch({error, _},
                   P(<<"[1, 2,">>)),
     ?_assertMatch({error, _},
                   P(<<"[ 1.0,2.0,]">>)),
     ?_assertMatch({error, _},
                   P(<<"[1.0, \"aa\",]">>))].

array_bind_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     right(
                       array(nullable(string())),
                       integer()),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {42, <<>>}},
                   P(<<"[]42">>)),
     ?_assertMatch({ok, {42, <<>>}},
                   P(<<"[\"aa\"]42">>)),
     ?_assertMatch({error, _},
                   P(<<"[1.0]42">>))].


array_mplus_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     mplus(
                       array(nullable(string())),
                       integer()),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok,{[], <<>>}},
                   P(<<"[]">>)),
     ?_assertMatch({ok,{42, <<>>}},
                   P(<<"42">>)),
     ?_assertMatch({error, _},
                   P(<<"[">>))].


float_array(Inp) ->
    ?s(to_parser(
         array(float()),
         ?r(Inp))).

float_list_test() ->
    ?assertMatch(
       {ok, {[42.0, 42.0, 4.2E14, 4.2E-11], <<>>}},
       float_array(<<"[42.0, 42 ,  42E13  ,42.0e-12  ]">>)). 

-define(_skip_json_assert(Bin),
        ?_assertMatch({ok, {ok, <<>>}}, P(Bin))).

skip_json_test_() ->
    P = fun(Inp) ->
                json_parsers:skip_json_p(Inp)
        end,
    [?_skip_json_assert(<<"{}">>),
     ?_skip_json_assert(<<"null">>),
     ?_skip_json_assert(<<"true">>),
     ?_skip_json_assert(<<"42">>),
     ?_skip_json_assert(<<"\"string\"">>),
     ?_skip_json_assert(<<"5.454e-32">>),
     ?_skip_json_assert(<<"{\"F1\": {\"F11\" : 42 , \"F12\" : [true, 25, null]},"
                       "\"F2\" :{}}">>)].

-define(_any_json_assert(Bin),
        ?_assertMatch({ok, {Bin, <<>>}}, P(Bin))).

any_json_test_() ->
    P = fun(Inp) ->
                json_parsers:any_json_p(Inp)
        end,
    [?_any_json_assert(<<"{}">>),
     ?_any_json_assert(<<"null">>),
     ?_any_json_assert(<<"true">>),
     ?_any_json_assert(<<"42">>),
     ?_any_json_assert(<<"\"string\"">>),
     ?_any_json_assert(<<"5.454e-32">>),
     ?_any_json_assert(<<"{\"F1\": {\"F11\" : 42 , \"F12\" : [true, 25, null]},
                \"F2\" :{}}">>)].

object_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     object(
                       [{?q("F1"), lift(?q(json_parsers:float_p)), ?q(1)},
                        {?q("F2"), lift(?q(json_parsers:string_p)), ?q(2)},
                        {?q("F3"), lift(?q(json_parsers:integer_p)), ?q(3)},
                        {?q("F4"), nullable(lift(?q(json_parsers:integer_p))),
                         ?q(4)}]),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {[], <<>>}}, P(<<"{}">>)),
     ?_assertMatch({ok, {[{3,6543},{2,<<"str">>},{1,42.23}], <<>>}},
                   P(<<"{\"F1\":42.23, \"F2\" : \"str\"  , \"F3\" :6543}">>)),
     ?_assertMatch({ok, {[{3,6543},{2,<<"str">>}], <<>>}},
                   P(<<"{ \"F2\" : \"str\",\n\r \"F3\" :6543\n\t\r\t}">>)),
     ?_assertMatch({error, _},
                   P(<<"{\"F1\":42.23, \"F2\" : }">>)),
     ?_assertMatch({ok, {[{4, 42}], <<>>}},
                   P(<<"{\"F4\" : 42}">>)),
     ?_assertMatch({ok, {[{4, undefined}], <<>>}},
                   P(<<"{\"F4\" : null}">>)),
     ?_assertMatch({error, _},
                   P(<<"{\"F1\" : true}">>)),
     {"With unknown fields",
      [?_assertMatch({ok, {[{1, 43.0}], <<>>}},
                     P(<<"{\"F1\" : 43, \"Unknown\" : {\"Ukn2\" : [1,true,{}]}}">>)),
      ?_assertMatch({error, _},
                    P(<<"{\"F1\" : 43, \"Unknown\" : {\"Ukn2\" : [1,true,{]}}">>))]}].

nested_object_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     object(
                       [{?q("F1"), lift(?q(json_parsers:float_p)), ?q(1)},
                        {?q("F2"), lift(?q(json_parsers:string_p)), ?q(2)},
                        {?q("F3"), object(
                                     [{?q("F31"),
                                       lift(?q(json_parsers:integer_p)),
                                       ?q(1)},
                                      {?q("F32"),
                                       array(lift(?q(json_parsers:string_p))),
                                       ?q(2)}]), ?q(3)},
                         {?q("F4"), array(
                                      object(
                                        [{?q("F41"),
                                          lift(?q(json_parsers:float_p)),
                                          ?q(1)}])), ?q(4)}]),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {[], <<>>}}, P(<<"{}">>)),
     ?_assertMatch({ok,
                    {[{4, [[{1, 410.0}], [{1, 4100.0}]]},
                      {3, [{2, [<<"32_1">>, <<"32_2">>]}, {1, 31}]},
                      {2, <<"2">>},
                      {1, 1.0}],
                     <<>>}},
                   P(<<"{\"F1\":1,"
                       "\"F2\":\"2\","
                       "\"F3\":{\"F31\":31,\"F32\":[\"32_1\", \"32_2\"]},"
                       "\"F4\":[{\"F41\":41E1},{\"F41\":41E2}]}">>))].
