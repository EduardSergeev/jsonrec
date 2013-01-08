-module(parsers).

-include_lib("meta/include/meta.hrl").

-export([return/1, bind/2, fail/1,
         mplus/2,
         left/2, right/2,
         get_bin/0,
         lift/1,
         match/1, matches/1, matches/2,
         guard/1,
         many_acc/2,
         many/1,
         sep_by_till_fold/5, sep_by_till_fold1/5,
         sep_by_till_acc/4, sep_by_till_acc1/4,
         sep_by_till1/3, sep_by_till/3,
         skip_many/1, skip_many1/1,
         count/2,
         option/2,

         to_parser/2,

         sequence/1]).


-type input() :: binary().
-type success(Value) :: {ok, {Value, input()}}.
-type failure() :: {error, any()}.
-type parser(Val) :: fun((input()) -> success(Val) | failure()).


-type q_parser(Val) ::
        fun((q_input(), q_success_cont(Val), q_failure_cont(Val)) ->
                   q_parser_fun(Val)).
-type q_input() :: meta:quote(input()).
-type q_value() :: meta:quote(any()).
-type q_failure() :: meta:quote(any()).

-type q_success_cont(Val) :: fun((q_input(), q_value()) -> q_parser(Val)).
-type q_failure_cont(Val) :: fun((q_failure()) -> q_parser(Val)).

-type q_parser_fun(Val) :: meta:quote(Val).

-export_type([input/0, success/1, failure/0,
              q_parser/1,
              parser/1]).


-define(re(Syntax), erl_syntax:revert(Syntax)).

%%
%% Basic monadic interface
%%
-spec return(meta:quote(Val)) -> q_parser(Val).
return(QVal) ->
    fun(QBin, Success, _Failure) ->  
            Success(QVal, QBin)
    end.

-spec bind(q_parser(A), fun((meta:quote(A)) -> q_parser(B))) ->
                  q_parser(B).
bind(Parser, Fun) ->
    fun(QBin, Success, Failure) ->
            Parser(QBin,
                   fun(QVal, QBin1) ->
                           Parser1 = Fun(QVal),
                           Parser1(QBin1, Success, Failure)
                   end,
                   Failure)
    end.

-spec fail(meta:quote(Error)) -> q_parser(any()) when
      Error :: any().
fail(QErr) ->
    fun(_QBin, _Success, Failure) ->  
            Failure(QErr)
    end.
    

%%
%% Our parser builder is also MonadPlus
%%
-spec mplus(q_parser(A), q_parser(B)) -> q_parser(A | B).
mplus(Left, Right) ->
    fun(QBin, Success, Failure) ->
            ?q(case
                   case ?s(Left(
                             QBin,
                             fun(QVal1, QBin1) ->
                                     ?q({ok, {?s(QVal1), ?s(QBin1)}})
                             end,
                             fun(QErr1) ->
                                     ?q({error, ?s(QErr1)})
                             end)) of
                       {error, _Err1} ->
                           ?s(Right(
                                QBin,
                                fun(QVal2, QBin3) ->
                                        ?q({ok, {?s(QVal2), ?s(QBin3)}})
                                end,
                                fun(QErr2) ->
                                        ?q({error, {all_failed, {?s(?r(_Err1)), ?s(QErr2)}}})
                                end));
                       OK ->
                           OK
                   end of
                   {ok, {_Val, _Bin1}} ->
                       ?s(Success(?r(_Val), ?r(_Bin1)));
                   {error, _Err2} ->
                       ?s(Failure(?r(_Err2)))
               end)
    end.


%%
%% Primitive parser combinators
%%
-spec get_bin() -> q_parser(input()).                     
get_bin() ->
    fun(QBin, Success, _Failure) ->
            Success(QBin, QBin)
    end.


-spec lift(meta:quote(parser(Val))) -> q_parser(Val).
lift(QParser) ->
    fun(QBin, Success, Failure) ->
            ?q(case ?s(QParser)(?s(QBin)) of
                   {ok, {_Val, _Bin1}} ->
                       ?s(Success(?r(_Val), ?r(_Bin1)));
                   {error, _Err} ->
                       ?s(Failure(?r(_Err)))
               end)
    end.

-spec match(meta:quote(Char)) -> q_parser(Char) when
      Char :: char() | string().
match(QC) ->
    fun(QBin, Success, Failure) ->
            ?q(case ?s(QBin) of
                   <<?s(QC), _Rest/binary>> ->
                       ?s(Success(QC, ?r(_Rest)));
                   _ ->
                       ?s(Failure(?q({expected, <<?s(QC)>>, at, ?s(QBin)})))
               end)
    end.

matches(QCs) ->
    matches(QCs, QCs).

-spec matches([meta:quote(Char)], [meta:quote(Result)]) ->
                     q_parser(Result) when
      Char :: char() | string(),
      Result :: any().
matches(QCs, QRs) ->
    fun(QBin, Success, Failure) ->
            QFs = [ ?q(fun(<<?s(QC), Rest/binary>>) ->
                               {ok, {?s(QR), Rest}}
                       end)
                    || {QC,QR} <- lists:zip(QCs, QRs) ],
            QD = ?q(fun(_) -> error end),
            Fun = fun(Arg, Qs) ->
                          Cs = lists:flatmap(
                                 fun erl_syntax:fun_expr_clauses/1,
                                 Qs),            
                          ?v(?re(erl_syntax:case_expr(Arg, Cs)))
                  end,
            ?q(case ?s(Fun(?i(QBin), ?i(sequence(QFs ++ [QD])))) of
                   {ok,  {_Val, _Bin}} ->
                       ?s(Success(?r(_Val), ?r(_Bin)));
                   error ->
                       ?s(Failure(?q({unexpected, at, ?s(QBin)})))
               end)
    end.

-spec guard(meta:quote(fun((Char) -> boolean()))) ->
                   q_parser(Char) when
      Char :: char() | string().
guard(QExpFun) ->
    fun(QBin, Success, Failure) ->
            ?q(case ?s(QBin) of
                   <<C/utf8, Rest/binary>> when ?s(QExpFun(?r(C))) ->
                       ?s(Success(?r(C), ?r(Rest)));
                   _ ->
                       ?s(Failure(?q({does_not_satisfy, at, ?s(QBin)})))
               end)
    end.

-spec many(q_parser(Val)) -> q_parser([Val]).
many(Parser) ->
    fun(QBin, Success, _) ->
            ?q(begin
                   Step = 
                       fun(Bin, Next, Acc) ->
                               ?s(Parser(
                                    ?r(Bin),
                                    fun(QVal, QBin1) ->
                                            ?q(?s(?r(Next))(
                                                   ?s(QBin1),
                                                   ?s(?r(Next)),
                                                   [?s(QVal)|?s(?r(Acc))]))
                                    end,
                                    fun(_QErr) ->
                                            ?q({lists:reverse(?s(?r(Acc))),
                                                ?s(?r(Bin))})
                                    end))
                       end,
                   {_Vals, _BinN} = Step(?s(QBin), Step, []),
                   ?s(Success(?r(_Vals), ?r(_BinN)))
               end)
    end.


-spec many_acc(q_parser(Val), meta:quote([Val])) ->
                      q_parser([Val]).
many_acc(Parser, QAcc) ->
    fun(QBin, Success, _) ->
            ?q(begin
                   Step = 
                       fun(Bin, Next, Acc) ->
                               ?s(Parser(
                                    ?r(Bin),
                                    fun(QVal, QBin1) ->
                                            ?q(?s(?r(Next))(
                                                   ?s(QBin1),
                                                   ?s(?r(Next)),
                                                   [?s(QVal)|?s(?r(Acc))]))
                                    end,
                                    fun(_QErr) ->
                                            ?q({?s(?r(Acc)),
                                                ?s(?r(Bin))})
                                    end))
                       end,
                   {_Vals, _BinN} = Step(?s(QBin), Step, ?s(QAcc)),
                   ?s(Success(?r(_Vals), ?r(_BinN)))
               end)
    end.

-spec skip_many(q_parser(any())) -> q_parser(ok).
skip_many(Parser) ->
    fun(QBin, Success, _) ->
            ?q(begin
                   Step = 
                       fun(Bin, Next) ->
                               ?s(Parser(
                                    ?r(Bin),
                                    fun(_QVal, QBin1) ->
                                            ?q(?s(?r(Next))(
                                                   ?s(QBin1),
                                                   ?s(?r(Next))))
                                    end,
                                    fun(_QErr) ->
                                            Success(?q(ok), ?r(Bin))
                                    end))
                       end,
                   Step(?s(QBin), Step)
               end)
    end.

-spec skip_many1(q_parser(any())) -> q_parser(ok).
skip_many1(Parser) ->
    fun(QBin, Success, Failure) ->
            ?q(begin
                   Step = 
                       fun(Bin, Next, First) ->
                               ?s(Parser(
                                    ?r(Bin),
                                    fun(_QVal, QBin1) ->
                                            ?q(?s(?r(Next))(
                                                   ?s(QBin1),
                                                   ?s(?r(Next)),
                                                   false))
                                    end,
                                    fun(QErr) ->
                                            ?q(if
                                                   ?s(?r(First)) ->
                                                       ?s(Failure(QErr));
                                                   true ->
                                                       ?s(Success(?q(ok), ?r(Bin)))
                                               end)
                                    end))
                       end,
                   Step(?s(QBin), Step, true)
               end)
    end.

-spec count(meta:quote(integer()), q_parser(Val)) ->
                   q_parser([Val]) when
      Val :: any().
count(QN, Parser) ->
    fun(QBin, Success, Failure) ->
            ?q(begin
                   Step = 
                       fun(_Bin1, _, _Acc1, I) when I =< 0 ->
                               ?s(Success(
                                    ?q(lists:reverse(?s(?r(_Acc1)))),
                                    ?r(_Bin1)));
                          (_Bin2, Next, _Acc2, I) ->
                               ?s(Parser(
                                    ?r(_Bin2),
                                    fun(QVal, QBin1) ->
                                            ?q(?s(?r(Next))(
                                                   ?s(QBin1),
                                                   ?s(?r(Next)),
                                                   [?s(QVal)|?s(?r(_Acc2))],
                                                   ?s(?r(I)) - 1))
                                    end,
                                    fun(QErr) ->
                                            Failure(QErr)
                                    end))
                       end,
                   Step(?s(QBin), Step, [], ?s(QN))
               end)
    end.

-spec sep_by_till(q_parser(Val), q_parser(Sep), q_parser(End)) ->
                         q_parser([Val]) when
      Val :: any(), Sep :: any(), End :: any().
sep_by_till(Parser, Sep, End) ->
    mplus(
      sep_by_till1(Parser, Sep, End),
      right(
        End,
        return(?q([])))).

-spec sep_by_till1(q_parser(Val), q_parser(Sep), q_parser(End)) ->
                         q_parser([Val]) when
      Val :: any(), Sep :: any(), End :: any().
sep_by_till1(Parser, Sep, End) ->
    bind(
      sep_by_till_acc1(Parser, Sep, End, ?q([])),
      fun(QVs) ->
              return(?q(lists:reverse(?s(QVs))))
      end).

%%--------------------------------------------------------------------
%% Similar to `parsers:sep_by_till/3` but instead of `[]`
%% this function uses provided accumulator `QAcc` (quote)
%% @end
%%--------------------------------------------------------------------
-spec sep_by_till_acc(q_parser(Val), q_parser(Sep), q_parser(End), QAcc) ->
                             q_parser([Val]) when
      Val :: any(), Sep :: any(), End :: any(),
      QAcc :: meta:quote([Val]).
sep_by_till_acc(Parser, Sep, End, QAcc) ->
    mplus(
      sep_by_till_acc1(Parser, Sep, End, QAcc),
      right(
        End,
        return(QAcc))).

-spec sep_by_till_acc1(q_parser(Val), q_parser(Sep), q_parser(End), QAcc) ->
                             q_parser([Val]) when
      Val :: any(), Sep :: any(), End :: any(),
      QAcc :: meta:quote([Val]).
sep_by_till_acc1(Parser, Sep, End, QAcc) ->
    QFun = fun(QV, QAcc1) ->
                   ?q([?s(QV)|?s(QAcc1)])
           end,
    sep_by_till_fold1(Parser, Sep, End, QFun, QAcc).


sep_by_till_fold(Parser, Sep, End, QFun, QAcc) ->
    mplus(
      sep_by_till_fold1(Parser, Sep, End, QFun, QAcc),
      right(
        End,
        return(QAcc))).

sep_by_till_fold1(Parser, Sep, End, QFun, QAcc) ->
    fun(QBin, Success, Failure) ->
            Parser1 = fun(QAcc1) ->
                              bind(
                                Parser,
                                fun(QV) ->
                                        return(QFun(QV, QAcc1))
                                end)
                      end,
            ?q(begin
                   Step = 
                       fun(Bin, Next, _Acc) ->
                               ?s((Parser1(?r(_Acc)))(
                                    ?r(Bin),
                                    fun(QAcc1, QBin1) ->
                                            Sep(
                                              QBin1,
                                              fun(_QSepVal, QBin2) ->
                                                      ?q(?s(?r(Next))(
                                                             ?s(QBin2),
                                                             ?s(?r(Next)),
                                                             ?s(QAcc1)))
                                              end,
                                              fun(QErr2) ->
                                                      End(
                                                        QBin1,
                                                        fun(_QEndVal, QBin3) ->
                                                                Success(QAcc1, QBin3)
                                                        end,
                                                        fun(QErr3) ->
                                                                Failure(
                                                                  ?q({sep_end_failed, 
                                                                      {?s(QErr2), ?s(QErr3)}}))
                                                        end)
                                              end)
                                    end,
                                    Failure))
                       end,
                   Step(?s(QBin), Step, ?s(QAcc))
               end)
    end. 

-spec option(q_parser(Val), Val) -> q_parser(Val) when
      Val :: any().
option(Parser, Default) ->
    mplus(Parser, return(Default)).

-spec left(q_parser(A), q_parser(any())) -> q_parser(A) when
      A :: any().
left(Left, Right) ->
    bind(Left,
         fun(Val) ->
                 bind(Right,
                      fun(_) ->
                              return(Val)
                      end)
         end).

-spec right(q_parser(any()), q_parser(A)) -> q_parser(A) when
      A :: any().
right(Left, Right) ->
    bind(Left,
         fun(_) ->
                 Right
         end).

%%
%% Additional monadic functions
%%
-spec sequence([meta:quote(Q)]) -> meta:quote([Q]) when
      Q :: any().
sequence([]) ->
    ?v([]);
sequence([Q|Qs]) ->
    ?v([?s(Q)|?s(sequence(Qs))]).



%%
%% Utilify function for conversion of meta-parser to parser body (as a quote)
%%
-spec to_parser(q_parser(Val), q_input()) -> parser(Val).
to_parser(Parser, QBin) ->
    Parser(
      QBin,
      fun(QVal, QBin1) ->
              ?q({ok, {?s(QVal), ?s(QBin1)}})
      end,
      fun(QErr) ->
              ?q({error, ?s(QErr)})
      end).
