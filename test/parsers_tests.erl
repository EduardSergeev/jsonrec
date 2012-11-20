-module(parsers_tests).

-include("../src/parsers.hrl").
-include_lib("eunit/include/eunit.hrl").

return_test() ->
    P = fun(_Inp) ->
                ?s(inst_body(?r(_Inp),
                             return(?q(42))))
        end,
    ?assertMatch({ok, {42, 0}}, P(<<"Garbage">>)).    

bind_test() ->
    P = fun(_Inp) ->
                ?s(inst_body(?r(_Inp),
                             bind(return(?q(42)),
                                  fun(QV) ->
                                          return(?q(?s(QV) + 1))
                                  end)))
        end,
    ?assertMatch({ok, {43, 0}}, P(<<"Garbage">>)).

get_pos_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     ?r(Inp),
                     bind(
                       get_pos(),
                       fun(P0) ->
                               bind(
                                 many(match(?q($1))),
                                 fun(_) ->
                                         bind(
                                           get_pos(),
                                           fun(P1) ->
                                                   return(?q({?s(P0),?s(P1)}))
                                           end)
                                 end)
                       end)))
        end,
    [?_assertMatch({ok, {{0,0}, 0}}, P(<<>>)),
     ?_assertMatch({ok, {{0,0}, 0}}, P(<<"Rest">>)),
     ?_assertMatch({ok, {{0,1}, 1}}, P(<<"1Rest">>)),
     ?_assertMatch({ok, {{0,3}, 3}}, P(<<"111Rest">>)),
     ?_assertMatch({ok, {{0,1}, 1}}, P(<<"1">>))
    ].
               
    

match_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(?r(Inp),
                             match(?q($1))))
        end,
    [{"match success",
      ?_assertMatch({ok, {$1, 1}},
                   P(<<"1Rest">>))},
     {"match failure",
      ?_assertMatch({error, {_, 0}},
                    P(<<"2Rest">>))}].

bind_match_test() ->
    P = fun(Inp) ->
                ?s(inst_body(?r(Inp),
                             bind(match(?q($1)),
                                  fun(Q1) ->
                                          bind(match(?q($2)),
                                               fun(Q2) ->
                                                       return(?q({?s(Q1),?s(Q2)}))
                                               end)
                                  end)))
        end,
    ?assertMatch({ok, {{$1,$2}, 2}}, P(<<"12Rest">>)).

many_match_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(?r(Inp),
                             many(match(?q($1)))))
        end,
    [{"One match",
      ?_assertMatch({ok, {[$1], 1}}, P(<<"1Rest">>))},
     {"Zero match",
      ?_assertMatch({ok, {[], 0}}, P(<<"Rest">>))},
     {"Many matches",
      ?_assertMatch({ok, {[$1,$1,$1], 3}}, P(<<"111Rest">>))}].

bind_many_match_test() ->
    P = fun(Inp) ->
                ?s(inst_body(?r(Inp),
                             bind(many(match(?q($1))),
                                  fun(QVs) ->
                                          return(?q(length(?s(QVs))))
                                  end)))
        end,
    ?assertMatch({ok, {3, 3}}, P(<<"111Rest">>)).

matches_test() ->
    P = fun(Inp) ->
                ?s(inst_body(?r(Inp),
                             matches([?q($1),?q($2)])))
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
           ?s(inst_body(?r(Inp),
                        bind(many(guard(fun(QC) ->
                                                ?q(?s(QC) >= $0
                                                   andalso ?s(QC) =< $9)
                                        end)),
                             fun(QVs) ->
                                     return(?q(length(?s(QVs))))
                             end)))
           end,
    ?assertMatch({ok, {4, 4}}, P(<<"1024Rest">>)).

mplus_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(?r(Inp),
                             mplus(match(?q($1)),
                                   match(?q($2)))))
        end,
    [{"First match",
      ?_assertMatch({ok, {$1, 1}}, P(<<"1Rest">>))},
     {"First match EOF",
      ?_assertMatch({ok, {$1, 1}}, P(<<"1">>))},
     {"Second match",
      ?_assertMatch({ok, {$2, 1}}, P(<<"2Rest">>))},
     {"Failure",
      ?_assertMatch({error, {_, 0}}, P(<<"Rest">>))}].

skip_while_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(
                     ?r(Inp),
                     skip_while(
                      fun(QC) ->
                              ?q(?s(QC) =/= $1)
                      end)))
        end,
    [?_assertMatch({ok, {_, 0}}, P(<<>>)),
     ?_assertMatch({ok, {_, 0}}, P(<<"1">>)),
     ?_assertMatch({ok, {_, 2}}, P(<<"221Rest">>))].
    


integer_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(?r(Inp), parsers:integer()))
        end,
    [ gen_assert(E,PVal) ||
        {E,PVal} <-
            [{{ok,{42,2}}, P(<<"42">>)},
             {{ok,{42,2}}, P(<<"42a">>)},
             {{ok,{42,2}}, P(<<"42.">>)},
             {{ok,{-42,3}}, P(<<"-42">>)},
             {{ok,{0,1}}, P(<<"0">>)},
             {{ok,{0,2}}, P(<<"-0">>)},
             {{ok,{0,1}}, P(<<"01">>)},
             {{error, 0}, P(<<"a">>)},
             {{error, 0}, P(<<"">>)}] ].

float_test_() ->
    P = fun(Inp) ->
                ?s(inst_body(?r(Inp), parsers:float()))
        end,
    [ gen_assert(E,PVal) ||
        {E,PVal} <-
            [{{ok,{42.0,2}}, P(<<"42">>)},
             {{ok,{42.0,2}}, P(<<"42a">>)},
             {{ok,{42.0,4}}, P(<<"42.0">>)},
             {{ok,{-42.0,3}}, P(<<"-42">>)},
             {{ok,{-42.0,3}}, P(<<"-42a">>)},
             {{ok,{-42.0,5}}, P(<<"-42.0">>)},
             {{ok,{42.0,2}}, P(<<"42.">>)},
             {{ok,{0.0,1}}, P(<<"0">>)},
             {{ok,{0.0,2}}, P(<<"-0">>)},
             {{ok,{0.0,1}}, P(<<"01">>)},
             {{ok,{42.0E35,7}}, P(<<"42.0E35">>)},
             {{ok,{-42.0E35,8}}, P(<<"-42.0E35">>)},
             {{ok,{42.0E-35,8}}, P(<<"42.0E-35">>)},
             {{ok,{-42.0E-35,9}}, P(<<"-42.0E-35">>)},
             {{ok,{-42.0E35,8}}, P(<<"-42.0E35">>)},
             {{ok,{42.1234e12,10}}, P(<<"42.1234E12">>)},
             {{ok,{42.0E35,5}}, P(<<"42E35">>)},
             {{ok,{42.0,4}}, P(<<"42E0">>)},
             {{ok,{420.0,4}}, P(<<"42E1">>)},
             {{error, 0}, P(<<"a">>)},
             {{error, 0}, P(<<"">>)},
             {{error, 0}, P(<<".E0">>)}] ].


float_list(Inp) ->
    ?s(inst_body(
         ?r(Inp),
         many(bind(parsers:float(),
                   fun(P) ->
                           bind(many(match(?q($ ))),
                                fun(_) ->
                                        bind(option(match(?q($,)),
                                                    ?q(undefined)),
                                             fun(_) ->
                                                     bind(many(match(?q($ ))),
                                                          fun(_) ->
                                                                  return(P)
                                                          end)
                                             end)
                                end)
                   end)))).

float_list_test() ->
    ?assertMatch(
       {ok, {[42.0, 42.0, 4.2E14, 4.2E-11], _}},
       float_list(<<"42.0, 42 ,  42E13  ,42.0e-12  ">>)). 

%%
%% Parser result _asser generator
%%
gen_assert({ok, _} = Success, P) ->
    ?_assertEqual(Success, P);
gen_assert({error, Pos}, P) ->
    ?_assertMatch({error, {_, Pos}}, P).
