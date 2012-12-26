-module(parsers_tests).

-include("../src/parsers.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([{nowarn_unused_function, [thousands/0]}]).


return_test() ->
    P = fun(_Inp) ->
                ?s(to_parser(
                     return(?q(42)),
                     ?r(_Inp)))
        end,
    ?assertMatch({ok, {42, <<"Garbage">>}},
                 P(<<"Garbage">>)).    

bind_test() ->
    P = fun(_Inp) ->
                ?s(to_parser(
                     bind(return(?q(42)),
                          fun(QV) ->
                                  return(?q(?s(QV) + 1))
                          end),
                     ?r(_Inp)))
        end,
    ?assertMatch({ok, {43, <<"Garbage">>}},
                 P(<<"Garbage">>)).


match_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     match(?q($1)),
                     ?r(Inp)))
        end,
    [{"match success",
      ?_assertMatch({ok, {$1, <<"Rest">>}},
                   P(<<"1Rest">>))},
     {"match failure",
      ?_assertMatch({error, {expected, <<"1">>, at, <<"2Rest">>}},
                    P(<<"2Rest">>))}].

match_string_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     match(?q("str")),
                     ?r(Inp)))
        end,
    [{"match success",
      ?_assertMatch({ok, {"str", <<"Rest">>}},
                   P(<<"strRest">>))},
     {"match failure",
      ?_assertMatch({error, _},
                    P(<<"trRest">>))}].

bind_match_test() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     bind(match(?q($1)),
                          fun(Q1) ->
                                  bind(match(?q($2)),
                                       fun(Q2) ->
                                               return(?q({?s(Q1),?s(Q2)}))
                                       end)
                          end),
                     ?r(Inp)))
        end,
    ?assertMatch({ok, {{$1,$2}, <<"Rest">>}},
                 P(<<"12Rest">>)).

many_match_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     many(match(?q($1))),
                     ?r(Inp)))
        end,
    [{"One match",
      ?_assertMatch({ok, {[$1], <<"Rest">>}},
                    P(<<"1Rest">>))},
     {"Zero match",
      ?_assertMatch({ok, {[], <<"Rest">>}},
                    P(<<"Rest">>))},
     {"Many matches",
      ?_assertMatch({ok, {"111", <<"Rest">>}},
                    P(<<"111Rest">>))}].

bind_many_match_test() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     bind(many(match(?q($1))),
                          fun(QVs) ->
                                  return(?q(length(?s(QVs))))
                          end),
                     ?r(Inp)))
        end,
    ?assertMatch({ok, {3, <<"Rest">>}},
                 P(<<"111Rest">>)).

matches_test() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     matches([?q($1),?q($2)]),
                     ?r(Inp)))
        end,
    [{"First match",
      ?_assertMatch({ok, {$1, <<"Rest">>}},
                    P(<<"1Rest">>))},
     {"First match EOF",
      ?_assertMatch({ok, {$1, <<>>}},
                    P(<<"1">>))},
     {"Second match",
      ?_assertMatch({ok, {$2, <<"Rest">>}},
                    P(<<"2Rest">>))},
     {"Failure",
      ?_assertMatch({error, _},
                    P(<<"Rest">>))}].    


guard_test() ->
    P = fun(Inp) ->
           ?s(to_parser(
                bind(many(guard(fun(QC) ->
                                        ?q(?s(QC) >= $0
                                           andalso ?s(QC) =< $9)
                                end)),
                     fun(QVs) ->
                             return(?q(length(?s(QVs))))
                     end),
                ?r(Inp)))
           end,
    ?assertMatch({ok, {4, <<"Rest">>}},
                 P(<<"1024Rest">>)).

mplus_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     mplus(match(?q($1)),
                           match(?q($2))),
                     ?r(Inp)))
        end,
    [{"First match",
      ?_assertMatch({ok, {$1, <<"Rest">>}},
                    P(<<"1Rest">>))},
     {"First match EOF",
      ?_assertMatch({ok, {$1, <<>>}},
                    P(<<"1">>))},
     {"Second match",
      ?_assertMatch({ok, {$2, <<"Rest">>}},
                    P(<<"2Rest">>))},
     {"Failure",
      ?_assertMatch({error, _},
                    P(<<"Rest">>))}].

skip_many_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     skip_many(match(?q($1))),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {_, <<>>}},
                   P(<<>>)),
     ?_assertMatch({ok, {_, <<>>}},
                   P(<<"1">>)),
     ?_assertMatch({ok, {_, <<"Rest">>}},
                   P(<<"111Rest">>))].

thousands() ->
    bind(match(?q($1)),
         fun(_) ->
                 many(match(?q($0)))
         end).

skip_many_longer_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     skip_many(
                       thousands()),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {_, <<>>}}, P(<<>>)),
     ?_assertMatch({ok, {_, <<"Rest">>}}, P(<<"10100Rest">>)),
     ?_assertMatch({ok, {_, <<"Rest">>}}, P(<<"111Rest">>))].

many_skip_many_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     many(
                       bind(
                         match(?q($1)),
                         fun(_) ->
                                 skip_many1(
                                   match(?q($0)))
                         end)),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {_, <<>>}}, P(<<>>)),
     ?_assertMatch({ok, {_, <<"Rest">>}}, P(<<"10Rest">>)),
     ?_assertMatch({ok, {_, <<"Rest">>}}, P(<<"10100Rest">>))].

ws_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     skip_many(
                      parsers:whitespace()),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {ok, <<"">>}}, P(<<"">>)),
     ?_assertMatch({ok, {ok, <<"1">>}}, P(<<"   1">>)),
     ?_assertMatch({ok, {ok, <<"1 ">>}}, P(<<"\n\t \r 1 ">>))].

boolean_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     parsers:boolean(),
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
                     parsers:integer(),
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
                     parsers:float(),
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
                     parsers:string(),
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
                     parsers:array(
                       parsers:nullable(
                         parsers:float())),
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
                       parsers:array(
                         parsers:nullable(
                           parsers:string())),
                       parsers:integer()),
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
                       parsers:array(
                         parsers:nullable(
                           parsers:string())),
                       parsers:integer()),
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
         parsers:array(parsers:float()),
         ?r(Inp))).

float_list_test() ->
    ?assertMatch(
       {ok, {[42.0, 42.0, 4.2E14, 4.2E-11], <<>>}},
       float_array(<<"[42.0, 42 ,  42E13  ,42.0e-12  ]">>)). 

-define(_skip_json_assert(Bin),
        ?_assertMatch({ok, {ok, <<>>}}, P(Bin))).

skip_json_test_() ->
    P = fun(Inp) ->
                parsers:skip_json_p(Inp)
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
                parsers:any_json_p(Inp)
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
                     parsers:object(
                       [{?q("F1"), lift(?q(parsers:float_p)), ?q(1)},
                        {?q("F2"), lift(?q(parsers:string_p)), ?q(2)},
                        {?q("F3"), lift(?q(parsers:integer_p)), ?q(3)},
                        {?q("F4"), parsers:nullable(lift(?q(parsers:integer_p))),
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
                     parsers:object(
                       [{?q("F1"), lift(?q(parsers:float_p)), ?q(1)},
                        {?q("F2"), lift(?q(parsers:string_p)), ?q(2)},
                        {?q("F3"), parsers:object(
                                     [{?q("F31"),
                                       lift(?q(parsers:integer_p)),
                                       ?q(1)},
                                      {?q("F32"),
                                       parsers:array(lift(?q(parsers:string_p))),
                                       ?q(2)}]), ?q(3)},
                         {?q("F4"), parsers:array(
                                     parsers:object(
                                      [{?q("F41"),
                                        lift(?q(parsers:float_p)),
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
