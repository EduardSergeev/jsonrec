-module(parsers).

-include_lib("meta/include/meta.hrl").

-compile(export_all).

-define(re(Syntax), erl_syntax:revert(Syntax)).

%%
%% Basic monadic interface
%%
return(QVal) ->
    fun(_QBin, QPos, Success, _Failure) ->  
            Success(QVal, QPos)
    end.

bind(Parser, Fun) ->
    fun(QBin, QPos, Success, Failure) ->
            Parser(QBin,
                   QPos,
                   fun(QVal, QPos1) ->
                           Parser1 = Fun(QVal),
                           Parser1(QBin, QPos1, Success, Failure)
                   end,
                   Failure)
    end.

%%
%% Our parser builder is also MonadPlus
%%
mplus(Left, Right) ->
    fun(QBin, QPos, Success, Failure) ->
            ?q(case ?s(Left(QBin,
                            QPos,
                            fun(QVal1, QPos1) ->
                                    ?q({ok, {?s(QVal1), ?s(QPos1)}})
                            end,
                            fun(_QErr1, _QPos1) ->
                                    Right(QBin,
                                          QPos,
                                          fun(QVal2, QPos2) ->
                                                  ?q({ok, {?s(QVal2), ?s(QPos2)}})
                                          end,
                                          Failure)
                            end)) of
                   {ok, {Val, Pos}} ->
                       ?s(Success(?r(Val), ?r(Pos)));
                   Err ->
                       Err
               end)
    end.
    

%%
%% Primitive parser combinators
%%

match(QC) ->
    fun(QBin, QPos, Success, Failure) ->
            ?q(case ?s(QBin) of
                   <<_:?s(QPos)/binary, ?s(QC), _/binary>> ->
                       Pos = ?s(QPos) + 1,
                       ?s(Success(QC, ?r(Pos)));
                   _ ->
                       ?s(Failure(?q({expected, <<?s(QC)>>}), QPos))
               end)
    end.

matches(QCs) when is_list(QCs) ->
    fun(QBin, QPos, Success, Failure) ->
            QFs = [ ?q(fun(<<_:?s(QPos)/binary, ?s(QC), _/binary>>) ->
                               {ok, ?s(QC)}
                       end)
                    || QC <- QCs ],
            QD = ?q(fun(_) -> error end),
            Fun = fun(Arg, Qs) ->
                          Cs = lists:flatmap(
                                 fun erl_syntax:fun_expr_clauses/1,
                                 Qs),            
                          ?v(?re(erl_syntax:case_expr(Arg, Cs)))
                  end,
            ?q(case ?s(Fun(?i(QBin), ?i(sequence(QFs ++ [QD])))) of
                   {ok, _Val} ->
                       Pos = ?s(QPos) + 1,
                       ?s(Success(?r(_Val), ?r(Pos)));
                   error ->
                       ?s(Failure(?q(unexpected), QPos))
               end)
    end.


guard(QExpFun) ->
    fun(QBin, QPos, Success, Failure) ->
            ?q(case ?s(QBin) of
                   <<_:?s(QPos)/binary, C, _/binary>> when ?s(QExpFun(?r(C))) ->
                       Pos = ?s(QPos) + 1,
                       ?s(Success(?r(C), ?r(Pos)));
                   _ ->
                       ?s(Failure(?q(does_not_satisfy), QPos))
               end)
    end.
    

many(Parser) ->
    fun(QBin, QPos, Success, _) ->
            QParser = ?q(fun(Pos) ->
                                 ?s(Parser(QBin,
                                           ?r(Pos),
                                           fun(QVal, QPos1) ->
                                                   ?q({ok, {?s(QVal), ?s(QPos1)}})
                                           end,
                                           fun(QError, QPos1) ->
                                                   ?q({error, {?s(QError), ?s(QPos1)}})
                                           end))
                         end),
            ?q(begin
                   {_Vals, Pos1} = ?MODULE:many_iter(?s(QParser), ?s(QPos), []),
                   ?s(Success(?r(_Vals), ?r(Pos1)))
               end)
    end.

many_iter(Parser, Pos, Acc) ->
    case Parser(Pos) of
        {ok, {Val, Pos1}} ->
            many_iter(Parser, Pos1, [Val|Acc]);
        {error, _} ->
            {lists:reverse(Acc), Pos}
    end.


fold(Parser, QFun, QAcc) ->
    fun(QBin, QPos, Success, _) ->
            QParser = ?q(fun(Pos) ->
                                 ?s(Parser(QBin,
                                           ?r(Pos),
                                           fun(QVal, QPos1) ->
                                                   ?q({ok, {?s(QVal), ?s(QPos1)}})
                                           end,
                                           fun(QError, QPos1) ->
                                                   ?q({error, {?s(QError), ?s(QPos1)}})
                                           end))
                         end),
            ?q(begin
                   {_Vals, Pos1} = ?MODULE:fold_iter(?s(QParser), ?s(QPos), ?s(QFun), ?s(QAcc)),
                   ?s(Success(?r(_Vals), ?r(Pos1)))
               end)
    end.
    
fold_iter(Parser, Pos, Fun, Acc) ->
    case Parser(Pos) of
        {ok, {Val, Pos1}} ->
            fold_iter(Parser, Pos1, Fun, Fun(Val, Acc));
        {error, _} ->
            {Acc, Pos}
    end.


many_acc(Parser, QAcc) ->
    fun(QBin, QPos, Success, _) ->
            QParser = ?q(fun(Pos) ->
                                 ?s(Parser(QBin,
                                           ?r(Pos),
                                           fun(QVal, QPos1) ->
                                                   ?q({ok, {?s(QVal), ?s(QPos1)}})
                                           end,
                                           fun(QError, QPos1) ->
                                                   ?q({error, {?s(QError), ?s(QPos1)}})
                                           end))
                         end),
            ?q(begin
                   {_Vals, Pos1} = ?MODULE:many_acc_iter(?s(QParser), ?s(QPos), ?s(QAcc)),
                   ?s(Success(?r(_Vals), ?r(Pos1)))
               end)
    end.

many_acc_iter(Parser, Pos, Acc) ->
    case Parser(Pos) of
        {ok, {Val, Pos1}} ->
            many_acc_iter(Parser, Pos1, [Val|Acc]);
        {error, _} ->
            {Acc, Pos}
    end.



option(Parser, Default) ->
    mplus(Parser, return(Default)).


%%
%% Additional monadic functions
%%
sequence([]) ->
    ?v([]);
sequence([Q|Qs]) ->
    ?v([?s(Q)|?s(sequence(Qs))]).


%%
%% Utilify function for conversion of meta-parser to parser body (as a quote)
%%
to_parser(Parser) ->
    ?q(fun(_Bin) ->
               ?s(Parser(?r(_Bin),
                         ?q(0),
                         fun(QVal, QPos) ->
                                 ?q({ok, {?s(QVal), ?s(QPos)}})
                         end,
                         fun(QError, QPos) ->
                                 ?q({error, {?s(QError), ?s(QPos)}})
                         end))
       end).

inst_body(QBin, Parser) ->
    Parser(QBin,
           ?q(0),
           fun(QVal, QPos1) ->
                   ?q({ok, {?s(QVal), ?s(QPos1)}})
           end,
           fun(QError, QPos1) ->
                   ?q({error, {?s(QError), ?s(QPos1)}})
           end).

%%
%% Tests
%%
%% test1() ->
%%     ?s(to_parser(return(?q(42)))).

%% test2(_Bin) ->
%%     ?s(inst_body(?r(_Bin),
%%                  bind(return(?q(42)),
%%                       fun(QV) ->
%%                               return(?q(?s(QV) + 1))
%%                       end))).

%% test3(Inp) ->
%%     ?s(inst_body(?r(Inp), match(?q($1)))).

%% test4() ->
%%     ?s(to_parser(
%%          bind(match(?q($1)),
%%               fun(_) ->
%%                       match(?q($2))
%%               end))).

%% test5() ->
%%     ?s(to_parser(
%%          bind(match(?q($1)),
%%               fun(QV1) ->
%%                       bind(match(?q($2)),
%%                            fun(QV2) ->
%%                                    return(?q({?s(QV1),?s(QV2)}))
%%                            end)
%%               end))).
                      

%% test6(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  many(match(?q($1))))).

%% test7() ->
%%     ?s(to_parser(
%%          bind(many(match(?q($1))),
%%               fun(QVs) ->
%%                       return(?q(length(?s(QVs))))
%%               end))).

%% test8(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  bind(many(guard(fun(QC) ->
%%                                          ?q(?s(QC) >= $0 andalso ?s(QC) =< $9)
%%                                  end)),
%%                       fun(QVs) ->
%%                               return(?q(length(?s(QVs))))
%%                       end))).

%% test9(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  mplus(match(?q($1)),
%%                        match(?q($2))))).

%% test10(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  matches([?q($1),?q($2)]))).

%% test11() ->
%%     sequence([?q(A),?q(2)]).
             


%%
%% JSON parsers
%%
digit() ->
    guard(fun(QC) ->
                  ?q(?s(QC) >= $0 andalso ?s(QC) =< $9)
          end).

digit1_9() ->
    guard(fun(QC) ->
                  ?q(?s(QC) >= $1 andalso ?s(QC) =< $9)
          end).

digit0() ->
    match(?q($0)).

%% positive() ->
%%     bind(digit1_9(),
%%          fun(QD) ->
%%                  bind(many(digit()),
%%                       fun(QDs) ->
%%                               return(?q([?s(QD)|?s(QDs)]))
%%                       end)
%%          end).

positive(Acc) ->
    bind(digit1_9(),
         fun(QD) ->
                 many_acc(digit(), ?q([?s(QD)|?s(Acc)]))
         end).

%% int() ->
%%     bind(
%%       option(
%%         bind(match(?q($-)), fun(_) -> return(?q([$-])) end),
%%         ?q([])),
%%       fun(QS) ->
%%               bind(mplus(positive(),
%%                          bind(digit0(),
%%                               fun(_) -> return(?q([$0])) end)),
%%                    fun(QN) -> return(?q(?s(QS) ++ ?s(QN))) end)
%%       end).

int(Acc) ->
    bind(
      option(
        bind(match(?q($-)), fun(_) -> return(?q([$-|?s(Acc)])) end),
        Acc),
      fun(Acc1) ->
              mplus(positive(Acc1),
                    bind(digit0(),
                         fun(_) -> return(?q([$0|?s(Acc1)])) end))
      end).

%% frac() ->
%%     bind(match(?q($.)),
%%          fun(_) ->
%%                  bind(digit(),
%%                       fun(QD) ->
%%                               bind(many(digit()),
%%                                    fun(QDs) ->
%%                                            return(?q([$.,?s(QD)|?s(QDs)]))
%%                                    end)
%%                       end)
%%          end).

frac(Acc) ->
    bind(match(?q($.)),
         fun(_) ->
                 bind(digit(),
                      fun(QD) ->
                              many_acc(digit(), ?q([?s(QD),$.|?s(Acc)]))
                      end)
         end).

%% exp() ->    
%%     bind(e(),
%%          fun(E) ->
%%                  bind(digit(),
%%                       fun(D) ->
%%                               bind(many(digit()),
%%                                    fun(Ds) ->
%%                                            return(?q(?s(E) ++ [?s(D)|?s(Ds)]))
%%                                    end)
%%                       end)
%%          end).

exp(Acc) ->    
    bind(e(Acc),
         fun(Acc1) ->
                 bind(digit(),
                      fun(D) ->
                              many_acc(digit(), ?q([?s(D)|?s(Acc1)]))
                      end)
         end).

%% e() ->
%%     bind(matches([?q($e),?q($E)]),
%%          fun(_) ->
%%                  option(
%%                    bind(matches([?q($-),?q($+)]),
%%                         fun(S) ->
%%                                 return(?q([$E,?s(S)]))
%%                         end),
%%                    ?q([$E]))
%%          end).

e(Acc) ->
    bind(matches([?q($e),?q($E)]),
         fun(_) ->
                 option(
                   bind(matches([?q($-),?q($+)]),
                        fun(S) ->
                                return(?q([?s(S),$E|?s(Acc)]))
                        end),
                   ?q([$E|?s(Acc)]))
         end).
    
%% float_digits() ->
%%     bind(int(),
%%          fun(IDs) ->
%%                  bind(
%%                    mplus(
%%                      bind(exp(),
%%                           fun(E) ->
%%                                   return(?q([$.,$0|?s(E)]))
%%                           end),
%%                      bind(frac(),
%%                           fun(FDs) ->
%%                                   bind(option(exp(), ?q([])),
%%                                        fun(E) ->
%%                                                return(?q(?s(FDs) ++ ?s(E)))
%%                                        end) 
%%                           end)),
%%                    fun(FDs) ->
%%                            return(?q(?s(IDs) ++ ?s(FDs)))
%%                    end)
%%          end).

float_digits(Acc) ->
    bind(int(Acc),
         fun(IDs) ->
                 bind(
                   option(frac(IDs), ?q([$0,$.|?s(IDs)])),
                   fun(FDs) ->
                           option(exp(FDs), FDs)
                   end)
         end).


%% integer() ->
%%     bind(int(),
%%          fun(QDs) ->
%%                  return(?q(list_to_integer(?s(QDs))))
%%          end).

integer() ->
    bind(int(?q([])),
         fun(QDs) ->
                 return(?q(list_to_integer(
                             lists:reverse(?s(QDs)))))
         end).


%% float() ->
%%     bind(float_digits(),
%%          fun(QDs) ->
%%                  return(?q(list_to_float(?s(QDs))))
%%          end).

float() ->
    bind(float_digits(?q([])),
         fun(QDs) ->
                 return(?q(list_to_float(
                             lists:reverse(?s(QDs)))))
         end).
    
%% int_list() ->
%%     many(bind(integer(),
%%               fun(P) ->
%%                       bind(many(match(?q($ ))),
%%                            fun(_) ->
%%                                    return(P)
%%                            end)
%%               end)).

int_list() ->
    many(bind(integer(),
              fun(P) ->
                      bind(many(match(?q($ ))),
                           fun(_) ->
                                   return(P)
                           end)
              end)).

%% float_list() ->
%%     many(bind(float(),
%%               fun(P) ->
%%                       bind(many(match(?q($ ))),
%%                            fun(_) ->
%%                                    return(P)
%%                            end)
%%               end)).

float_list() ->
    many(bind(float(),
              fun(P) ->
                      bind(many(match(?q($ ))),
                           fun(_) ->
                                   return(P)
                           end)
              end)).


%%
%% Insts
%%

%% positive(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  positive())).

%% positive_a_p(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  positive_a(?q([])))).


%% int(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  int())).

%% integer2(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  integer_a())).

%% exp(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  exp())).

%% float(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  float())).

%% float_d_p(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  float_d_a(?q([])))).

int_list(Inp) ->
    ?s(inst_body(?r(Inp),
                 int_list())).

%% int_a_list(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  int_a_list())).

float_list(Inp) ->
    ?s(inst_body(?r(Inp),
                 float_list())).

%% float_a_list(Inp) ->
%%     ?s(inst_body(?r(Inp),
%%                  float_a_list())).

%% pos_parser(Inp) ->
%%     ?s(inst_body(?r(Inp), pos_list())).
