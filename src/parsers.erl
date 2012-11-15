-module(parsers).

-include_lib("meta/include/meta.hrl").

-compile(export_all).

-record(input, {bin, pos}).

%%-type quote() :: fun((any()) -> any()).
%%-type parser(A) :: fun((quote(), fun()) -> )

return(QVal) ->
    fun(QInp, Success, _Failure) ->  
            Success(QVal, QInp)
    end.

bind(Parser, Fun) ->
    fun(QInp, Success, Failure) ->
            Parser(QInp,
                   fun(QVal, QInp1) ->
                           Parser2 = Fun(QVal),
                           Parser2(QInp1, Success, Failure)
                   end,
                   Failure)
    end.

mplus(Left, Right) ->
    fun(QInp, Succ, Fail) ->
            Left(QInp,
                 Succ,
                 fun(_Err, _QInp1) ->
                         Right(QInp, Succ, Fail)
                 end)
    end.
    

to_parser(Parser) ->
    ?q(fun(Inp) ->
               ?s(Parser(?r(Inp),
                         fun(QVal, QInp) ->
                                 ?q({ok, {?s(QVal), ?s(QInp)}})
                         end,
                         fun(QError, QInp) ->
                                 ?q({error, {?s(QError), ?s(QInp)}})
                         end))
       end).

inst_body(QInp, Parser) ->
    Parser(QInp,
           fun(QVal, QInp1) ->
                   ?q({ok, {?s(QVal), ?s(QInp1)}})
           end,
           fun(QError, QInpq) ->
                   ?q({error, {?s(QError), ?s(QInpq)}})
           end).



%% match(QC) ->
%%     fun(QInp, Cont) ->  
%%             ?q(fun(#input{bin = <<?s(QC), Rest/binary>>, pos = Pos} = ?s(QInp)) ->
%%                        ?s(Cont(QC, ?q(#input{bin = ?s(?r(Rest)), pos = ?s(?r(Pos)) + 1})));
%%                   (_) ->
%%                        {error, {{expected, ?s(QC)}, ?s(QInp)}}
%%                end)
%%     end.

match(QC) ->
    fun(QInp, Succ, Fail) ->
            ?q(case ?s(QInp) of
                   #input{bin = <<?s(QC), Rest/binary>>, pos = Pos} ->
                       ?s(Succ(QC, ?q(#input{bin = ?s(?r(Rest)), pos = ?s(?r(Pos)) + 1})));
                   _ ->
                       ?s(Fail(?q({expected, <<?s(QC)>>}), QInp))
               end)
    end.

guard(QExpFun) ->
    fun(QInp, Succ, Fail) ->
            ?q(case ?s(QInp) of
                   #input{bin = <<C, Rest/binary>>, pos = Pos} when ?s(QExpFun(?r(C))) ->
                       ?s(Succ(?r(C), ?q(#input{bin = ?s(?r(Rest)), pos = ?s(?r(Pos)) + 1})));
                   _ ->
                       ?s(Fail(?q(does_not_satisfy), QInp))
               end)
    end.
    
many(Parser) ->
    fun(QInp, Succ, _Fail) ->
            QParser = ?q(fun(QPInp) ->
                                 ?s(Parser(?r(QPInp),
                                           fun(QVal, QInp2) ->
                                                   ?q({ok, {?s(QVal), ?s(QInp2)}})
                                           end,
                                           fun(QError, QInp2) ->
                                                   ?q({error, {?s(QError), ?s(QInp2)}})
                                           end))
                         end),
            ?q(begin
                   {_Vals, Inp} = ?MODULE:many_iter(?s(QParser), ?s(QInp), []),
                   ?s(Succ(?r(_Vals), ?r(Inp)))
               end)
    end.

    
many_iter(Parser, Inp, Acc) ->
    case Parser(Inp) of
        {ok, {Val, Inp1}} ->
            many_iter(Parser, Inp1, [Val|Acc]);
        {error, _} ->
            {lists:reverse(Acc), Inp}
    end.


fold(Parser, QFun, QAcc) ->
    fun(QInp, Succ, _Fail) ->
            QParser = ?q(fun(QPInp) ->
                                 ?s(Parser(?r(QPInp),
                                           fun(QVal, QInp2) ->
                                                   ?q({ok, {?s(QVal), ?s(QInp2)}})
                                           end,
                                           fun(QError, QInp2) ->
                                                   ?q({error, {?s(QError), ?s(QInp2)}})
                                           end))
                         end),
            ?q(begin
                   {_Vals, Inp} = ?MODULE:fold_iter(?s(QParser), ?s(QInp), ?s(QFun), ?s(QAcc)),
                   ?s(Succ(?r(_Vals), ?r(Inp)))
               end)
    end.

    
fold_iter(Parser, Inp, Fun, Acc) ->
    case Parser(Inp) of
        {ok, {Val, Inp1}} ->
            fold_iter(Parser, Inp1, Fun, Fun(Val, Acc));
        {error, _} ->
            {Acc, Inp}
    end.

%%
%% Tests
%%
test1() ->
    ?s(to_parser(return(?q(42)))).

test2() ->
    ?s(to_parser(
         bind(return(?q(42)),
              fun(QV) ->
                      return(?q(?s(QV) + 1))
              end))).

test3(Inp) ->
    ?s(inst_body(?r(Inp), match(?q($1)))).

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
                      

%% test6() ->
%%     ?s(to_parser(
%%          many(match(?q($1))))).

%% test7() ->
%%     ?s(to_parser(
%%          bind(many(match(?q($1))),
%%               fun(QVs) ->
%%                       return(?q(length(?s(QVs))))
%%               end))).

%% test8() ->
%%     ?s(to_parser(
%%          bind(many(guard(fun(QC) ->
%%                                  ?q(?s(QC) >= $0 andalso ?s(QC) =< $9)
%%                          end)),
%%               fun(QVs) ->
%%                       return(?q(length(?s(QVs))))
%%               end))).

test9(Inp) ->
    ?s(inst_body(?r(Inp),
                 mplus(match(?q($1)),
                       match(?q($2))))).
              
positive() ->
    bind(guard(fun(QC) ->
                       ?q(?s(QC) >= $1 andalso ?s(QC) =< $9)
               end),
         fun(QD) ->
                 bind(many(guard(fun(QC) ->
                                         ?q(?s(QC) >= $0 andalso ?s(QC) =< $9)
                                 end)),
                      fun(QDs) ->
                              return(?q(list_to_integer([?s(QD)|?s(QDs)])))
                      end)
         end).

%% positive() ->
%%     bind(guard(fun(QC) ->
%%                        ?q(?s(QC) >= $1 andalso ?s(QC) =< $9)
%%                end),
%%          fun(QD) ->
%%                  bind(fold(guard(fun(QC) ->
%%                                          ?q(?s(QC) >= $0 andalso ?s(QC) =< $9)
%%                                  end),
%%                            ?q(fun(D, Acc) -> D-$0 + Acc*10 end),
%%                            ?q(?s(QD)-$0)),
%%                       fun(QI) ->
%%                               return(QI)
%%                       end)
%%          end).

zero() ->
    bind(match(?q($0)),
         fun(_) ->
              return(?q(0))   
         end).

%% p_zero(Inp) ->
%%     ?s(inst_body(?r(Inp), zero())).

%% integ() ->
%%     bind(mplus(match(?q($-)),
%%                return(?q(undefined))),
%%          fun(S) ->
%%                  %% bind(positive(),
%%                  %%      fun(P) -> return(?q(-?s(P))) end)
%%                  ?q(case ?s(S) of
%%                         $- ->
%%                             ?s(bind(positive(),
%%                                     fun(P) -> return(?q(-?s(P))) end));
%%                         _ ->
%%                             ?s(mplus(zero(), positive()))
%%                     end)
%%          end).

integ() ->
    mplus(bind(match(?q($-)),
               fun(_) ->
                       bind(positive(),
                            fun(P) -> return(?q(-?s(P))) end)
               end),
          mplus(zero(), positive())).


pos_list() ->
    many(
      bind(integ(),
           fun(P) ->
                   bind(many(match(?q($ ))),
                        fun(_) ->
                                return(P)
                        end)
           end)).

pos_parser(Inp) ->
    ?s(inst_body(?r(Inp), pos_list())).
