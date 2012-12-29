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
