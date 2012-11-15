-module(parsers).

-include_lib("meta/include/meta.hrl").

-compile(export_all).

%%-record(input, {bin, pos}).

%%-type quote() :: fun((any()) -> any()).
%%-type parser(A) :: fun((quote(), fun()) -> )

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

mplus(Left, Right) ->
    fun(QBin, QPos, Success, Failure) ->
            Left(QBin,
                 QPos,
                 Success,
                 fun(_Err, _QPos1) ->
                         Right(QBin, QPos, Success, Failure)
                 end)
    end.
    

to_parser(Parser) ->
    ?q(fun(Bin) ->
               ?s(Parser(?r(Bin),
                         ?q(0),
                         fun(QVal, QPos) ->
                                 ?q({ok, {?s(QVal), ?s(QPos)}})
                         end,
                         fun(QError, QPos) ->
                                 ?q({error, {?s(QError), ?s(QPos)}})
                         end))
       end).

%% inst_body(QBin, Parser) ->
%%     ?q(begin
%%            Pos = 0,
%%            ?s(Parser(QBin,
%%                      ?r(Pos),
%%                      fun(QVal, QPos1) ->
%%                              ?q({ok, {?s(QVal), ?s(QPos1)}})
%%                      end,
%%                      fun(QError, QPos1) ->
%%                              ?q({error, {?s(QError), ?s(QPos1)}})
%%                      end))
%%        end).

inst_body(QBin, Parser) ->
    Parser(QBin,
           ?q(0),
           fun(QVal, QPos1) ->
                   ?q({ok, {?s(QVal), ?s(QPos1)}})
           end,
           fun(QError, QPos1) ->
                   ?q({error, {?s(QError), ?s(QPos1)}})
           end).



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

%%
%% Tests
%%
test1() ->
    ?s(to_parser(return(?q(42)))).

test2(Bin) ->
    ?s(inst_body(?r(Bin),
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
                      

test6(Inp) ->
    ?s(inst_body(?r(Inp),
                 many(match(?q($1))))).

%% test7() ->
%%     ?s(to_parser(
%%          bind(many(match(?q($1))),
%%               fun(QVs) ->
%%                       return(?q(length(?s(QVs))))
%%               end))).

test8(Inp) ->
    ?s(inst_body(?r(Inp),
                 bind(many(guard(fun(QC) ->
                                         ?q(?s(QC) >= $0 andalso ?s(QC) =< $9)
                                 end)),
                      fun(QVs) ->
                              return(?q(length(?s(QVs))))
                      end))).

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

positive(Inp) ->
    ?s(inst_body(?r(Inp),
                 positive())).

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

integ(Inp) ->
    ?s(inst_body(?r(Inp),
                 integ())).
    


int_list() ->
    many(bind(integ(),
              fun(P) ->
                      bind(many(match(?q($ ))),
                           fun(_) ->
                                   return(P)
                           end)
              end)).

int_list(Inp) ->
    ?s(inst_body(?r(Inp),
                 int_list())).

%% pos_parser(Inp) ->
%%     ?s(inst_body(?r(Inp), pos_list())).
