-module(parsers_tests).

-include("../src/parsers.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([{nowarn_unused_function, [thousands/0]}]).

return_test() ->
    P = fun(_Inp) ->
                ?s(inst_body(
                     return(?q(42)),
                     ?r(_Inp)))
        end,
    ?assertMatch({ok, {42, 0}}, P(<<"Garbage">>)).    

bind_test() ->
    P = fun(_Inp) ->
                ?s(inst_body(
                     bind(return(?q(42)),
                          fun(QV) ->
                                  return(?q(?s(QV) + 1))
                          end),
                     ?r(_Inp)))
        end,
    ?assertMatch({ok, {43, 0}}, P(<<"Garbage">>)).


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
      ?_assertMatch({error, {_, <<"2Rest">>}},
                    P(<<"2Rest">>))}].

match_string_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     match(?q("str")),
                     ?r(Inp)))
        end,
    [{"match success",
      ?_assertMatch({ok, {"str", 3}},
                   P(<<"strRest">>))},
     {"match failure",
      ?_assertMatch({error, {_, 0}},
                    P(<<"trRest">>))}].

bind_match_test() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     bind(match(?q($1)),
                          fun(Q1) ->
                                  bind(match(?q($2)),
                                       fun(Q2) ->
                                               return(?q({?s(Q1),?s(Q2)}))
                                       end)
                          end),
                     ?r(Inp)))
        end,
    ?assertMatch({ok, {{$1,$2}, 2}}, P(<<"12Rest">>)).

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
                ?s(inst_body(
                     bind(many(match(?q($1))),
                          fun(QVs) ->
                                  return(?q(length(?s(QVs))))
                          end),
                     ?r(Inp)))
        end,
    ?assertMatch({ok, {3, 3}}, P(<<"111Rest">>)).

matches_test() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     matches([?q($1),?q($2)]),
                     ?r(Inp)))
        end,
    [{"First match",
      ?_assertMatch({ok, {$1, 1}}, P(<<"1Rest">>))},
     {"First match EOF",
      ?_assertMatch({ok, {$1, 1}}, P(<<"1">>))},
     {"Second match",
      ?_assertMatch({ok, {$2, 1}}, P(<<"2Rest">>))},
     {"Failure",
      ?_assertMatch({error, {_, 0}}, P(<<"Rest">>))}].    


guard_test() ->
    P = fun(Inp) ->
           ?s(inst_body(
                bind(many(guard(fun(QC) ->
                                        ?q(?s(QC) >= $0
                                           andalso ?s(QC) =< $9)
                                end)),
                     fun(QVs) ->
                             return(?q(length(?s(QVs))))
                     end),
                ?r(Inp)))
           end,
    ?assertMatch({ok, {4, 4}}, P(<<"1024Rest">>)).

mplus_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     mplus(match(?q($1)),
                           match(?q($2))),
                     ?r(Inp)))
        end,
    [{"First match",
      ?_assertMatch({ok, {$1, 1}}, P(<<"1Rest">>))},
     {"First match EOF",
      ?_assertMatch({ok, {$1, 1}}, P(<<"1">>))},
     {"Second match",
      ?_assertMatch({ok, {$2, 1}}, P(<<"2Rest">>))},
     {"Failure",
      ?_assertMatch({error, {_, 0}}, P(<<"Rest">>))}].

skip_many_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     skip_many(match(?q($1))),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {_, 0}}, P(<<>>)),
     ?_assertMatch({ok, {_, 1}}, P(<<"1">>)),
     ?_assertMatch({ok, {_, 3}}, P(<<"111Rest">>))].

thousands() ->
    bind(match(?q($1)),
         fun(_) ->
                 many(match(?q($0)))
         end).

skip_many_longer_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     skip_many(thousands()),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {_, 0}}, P(<<>>)),
     ?_assertMatch({ok, {_, 5}}, P(<<"10100Rest">>)),
     ?_assertMatch({ok, {_, 3}}, P(<<"111Rest">>))].

many_skip_many_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     many(
                       bind(match(?q($1)),
                            fun(_) ->
                                    skip_many1(
                                      match(?q($0)))
                            end)),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {_, 0}}, P(<<>>)),
     ?_assertMatch({ok, {_, 2}}, P(<<"10Rest">>)),
     ?_assertMatch({ok, {_, 5}}, P(<<"10100Rest">>))].

ws_test_() ->
    P = fun(Inp) ->
                ?s(to_parser(
                     skip_many(
                      parsers:whitespace()),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {ok, <<"">>}},
                   P(<<"">>)),
     ?_assertMatch({ok, {ok, <<"1">>}},
                   P(<<"   1">>)),
     ?_assertMatch({ok, {ok, <<"1 ">>}},
                   P(<<"\n\t \r 1 ">>))].
    

boolean_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     parsers:boolean(),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {true, 4}}, P(<<"true">>)),
     ?_assertMatch({ok, {false, 5}}, P(<<"false">>)),
     ?_assertMatch({error, {_, 0}}, P(<<"tru">>)),
     ?_assertMatch({error, {_, 0}}, P(<<"tru1">>)),
     ?_assertMatch({ok, {true, 4}}, P(<<"true1">>)),
     ?_assertMatch({ok, {false, 5}}, P(<<"false1">>))].

integer_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     parsers:integer(),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok,{42, 2}},
                   P(<<"42">>)),
     ?_assertMatch({ok,{42, 2}},
                   P(<<"42a">>)),
     ?_assertMatch({ok,{42, 2}},
                   P(<<"42.">>)),
     ?_assertMatch({ok,{-42, 3}},
                   P(<<"-42">>)),
     ?_assertMatch({ok,{0, 1}},
                   P(<<"0">>)),
     ?_assertMatch({ok,{0, 2}},
                   P(<<"-0">>)),
     ?_assertMatch({ok,{0, 1}},
                   P(<<"01">>)),
     ?_assertMatch({error, {_, 0}},
                   P(<<"a">>)),
     ?_assertMatch({error, {_, 0}},
                   P(<<"">>))].

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
     ?_assertMatch({error, {_, <<"a">>}},
                   P(<<"a">>)),
     ?_assertMatch({error, {_, <<>>}},
                   P(<<"">>)),
     ?_assertMatch({error, {_, <<".E0">>}},
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
     ?_assertMatch({error, {_, <<>>}},
                   P(<<"\"">>)),
     ?_assertMatch({error, {_, <<"abc">>}},
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
     ?_assertMatch({ok,{[1.0, 2.0], <<>>}},
                   P(<<"[ 1.0,2.0,]">>)),
     ?_assertMatch({ok,{[1.0, undefined, 2.0], <<>>}},
                   P(<<"[1.0, null, 2.0]">>)),
     ?_assertMatch({error, {_, <<>>}},
                   P(<<"[1, 2,">>)),
     ?_assertMatch({error, {_, <<"\"aa\",]">>}},
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
     ?_assertMatch({error, {_, <<"1.0]42">>}},
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
     ?_assertMatch({error, {_, <<"[">>}},
                   P(<<"[">>))].


float_array(Inp) ->
    ?s(inst_body(
         parsers:array(parsers:float()),
         ?r(Inp))).

float_list_test() ->
    ?assertMatch(
       {ok, {[42.0, 42.0, 4.2E14, 4.2E-11], _}},
       float_array(<<"[42.0, 42 ,  42E13  ,42.0e-12  ]">>)). 

skip_json_test_() ->
    P = fun(Inp) ->
                parsers:skip_json_p(Inp)
        end,
    Assert = fun(Inp) ->
                     ?_assertMatch({ok, {_, <<>>}}, P(Inp))
             end,                
    [Assert(<<"{}">>),
     Assert(<<"null">>),
     Assert(<<"true">>),
     Assert(<<"42">>),
     Assert(<<"\"string\"">>),
     Assert(<<"5.454e-32">>),
     Assert(<<"{\"F1\": {\"F11\" : 42 , \"F12\" : [true, 25, null]},
                \"F2\" :{}}">>)].

any_json_test_() ->
    P = fun(Inp) ->
                parsers:any_json_p(Inp)
        end,
    Assert = fun(Inp) ->
                     ?_assertEqual({ok, {Inp, <<>>}}, P(Inp))
             end,                
    [Assert(<<"{}">>),
     Assert(<<"null">>),
     Assert(<<"true">>),
     Assert(<<"42">>),
     Assert(<<"\"string\"">>),
     Assert(<<"5.454e-32">>),
     Assert(<<"{\"F1\": {\"F11\" : 42 , \"F12\" : [true, 25, null]},
                \"F2\" :{}}">>)].

object_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     parsers:object(
                       [{?q("F1"), lift(?q(parsers:float_p)), ?q(1)},
                        {?q("F2"), lift(?q(parsers:string_p)), ?q(2)},
                        {?q("F3"), lift(?q(parsers:integer_p)), ?q(3)},
                        {?q("F4"), parsers:nullable(lift(?q(parsers:integer_p))),
                         ?q(4)}]),
                     ?r(Inp)))
        end,
    [?_assertMatch({ok, {[], _}}, P(<<"{}">>)),
     ?_assertMatch({ok, {[{1,42.23},{2,<<"str">>},{3,6543}], _}},
                   P(<<"{\"F1\":42.23, \"F2\" : \"str\"  , \"F3\" :6543}">>)),
     ?_assertMatch({ok, {[{2,<<"str">>},{3,6543}], _}},
                   P(<<"{ \"F2\" : \"str\",\n\r \"F3\" :6543\n\t,\t}">>)),
     ?_assertMatch({error, _},
                   P(<<"{\"F1\":42.23, \"F2\" : }">>)),
     ?_assertMatch({ok, {[{4, 42}], _}},
                   P(<<"{\"F4\" : 42}">>)),
     ?_assertMatch({ok, {[{4, undefined}], _}},
                   P(<<"{\"F4\" : null}">>)),
     {"With unknown fields",
      [?_assertMatch({ok, {[{1, 43.0}], _}},
                     P(<<"{\"F1\" : 43, \"Unknown\" : {\"Ukn2\" : [1,true,{}]}}">>)),
      ?_assertMatch({error, _},
                    P(<<"{\"F1\" : 43, \"Unknown\" : {\"Ukn2\" : [1,true,{]}}">>))]}].

nested_object_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
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
    [?_assertMatch({ok, {[], _}}, P(<<"{}">>)),
     ?_assertMatch({ok,
                    {[{1, 1.0},
                      {2, <<"2">>},
                      {3, [{1, 31},{2, [<<"32_1">>, <<"32_2">>]}]},
                      {4, [[{1, 410.0}], [{1, 4100.0}]]}], _}},
                   P(<<"{\"F1\":1,"
                       "\"F2\":\"2\","
                       "\"F3\":{\"F31\":31,\"F32\":[\"32_1\", \"32_2\"]},"
                       "\"F4\":[{\"F41\":41E1},{\"F41\":41E2}]}">>))].
