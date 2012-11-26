-module(parsers).

-include_lib("meta/include/meta.hrl").

%%-compile(export_all).
-export([return/1, bind/2, fail/1,
         mplus/2,
         get_pos/0, get_bin/0,
         lift/1,
         match/1, matches/1,
         guard/1,
         fold/3, fold_iter/4,
         many_acc/2, many_acc_iter/3,
         many/1, many1/1, many_iter/3,
         skip_many/1, skip_many1/1,
         skip_while/1,
         option/2,

         whitespace/0,
         nullable/1,
         boolean/0, integer/0, float/0, string/0,
         object/1,

         integer_p/2, float_p/2, string_p/2,

         inst_body/2]).

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

fail(QErr) ->
    fun(_QBin, QPos, _Success, Failure) ->  
            Failure(QErr, QPos)
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
                   {ok, {_Val, _Pos}} ->
                       ?s(Success(?r(_Val), ?r(_Pos)));
                   Err ->
                       Err
               end)
    end.
    

%%
%% Primitive parser combinators
%%

get_pos() ->
    fun(_QBin, QPos, Success, _Failure) ->
            Success(QPos, QPos)
    end.

get_bin() ->
    fun(QBin, QPos, Success, _Failure) ->
            Success(QBin, QPos)
    end.
            
lift(QParser) ->
    fun(QBin, QPos, Success, Failure) ->
            ?q(case ?s(QParser)(?s(QBin), ?s(QPos)) of
                   {ok, {_Val, _Pos1}} ->
                       ?s(Success(?r(_Val), ?r(_Pos1)));
                   {error, {_Err, _Pos2}} ->
                       ?s(Failure(?r(_Err), ?r(_Pos2)))
               end)
    end.


match(QC) ->
    fun(QBin, QPos, Success, Failure) ->
            ?q(case ?s(QBin) of
                   <<_:?s(QPos)/binary, ?s(QC), _/binary>> ->
                       Pos = ?s(QPos) + ?s(qsize(?i(QC))),
                       ?s(Success(QC, ?r(Pos)));
                   _ ->
                       ?s(Failure(?q({expected, <<?s(QC)>>}), QPos))
               end)
    end.

matches(QCs) ->
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

many1(Parser) ->
    bind(Parser,
         fun(V) ->
                 bind(many(Parser),
                      fun(Vs) ->
                              return(?q([?s(V)|?s(Vs)]))
                      end)
         end).

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

skip_while(QGuardFun) ->
    fun(QBin, QPos, Success, _) ->
            ?q(begin
                   SkipWhileIter =
                       fun(Pos, Next) ->
                               case ?s(QBin) of
                                   <<_:Pos/binary, C, _/binary>>
                                     when ?s(QGuardFun(?r(C))) ->
                                       Next(Pos + 1, Next);
                                   _ ->
                                       Pos
                               end
                       end,
                   Pos1 = SkipWhileIter(?s(QPos), SkipWhileIter),
                   ?s(Success(?r(Pos1), ?r(Pos1)))
               end)
    end.
    

skip_many(Parser) ->
    fun(QBin, QPos, Success, _) ->
            ?q(begin
                   Iter =
                       fun(PosFixMe, Next) ->
                               ?s(Parser(QBin, ?r(PosFixMe),
                                         fun(_, QPos1) ->
                                                 ?q(?s(?r(Next))(?s(QPos1), ?s(?r(Next))))
                                         end,
                                         fun(_, QPos1) ->
                                                 QPos1
                                         end))
                       end,
                   Pos1 = Iter(?s(QPos), Iter),
                   ?s(Success(?r(Pos1), ?r(Pos1)))
               end)
    end.

skip_many1(Parser) ->
    bind(Parser,
         fun(_) ->
                 skip_many(Parser)
         end).


option(Parser, Default) ->
    mplus(Parser, return(Default)).

left(Left, Right) ->
    bind(Left,
         fun(Val) ->
                 bind(Right,
                      fun(_) ->
                              return(Val)
                      end)
         end).

right(Left, Right) ->
    bind(Left,
         fun(_) ->
                 Right
         end).

%%
%% Utils
%%
qsize({string, _, S}) ->
    ?v(?re(erl_syntax:abstract(length(S))));
qsize({integer, _, _}) ->
    ?q(1);
qsize({char, _, _}) ->
    ?q(1);
qsize(E) ->
    error({unsupported_type, E}).

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
inst_body(QBin, Parser) ->
    inst_body(Parser, QBin, ?q(0)).

inst_body(Parser, QBin, QPos) ->
    Parser(QBin,
           QPos,
           fun(QVal, QPos1) ->
                   ?q({ok, {?s(QVal), ?s(QPos1)}})
           end,
           fun(QError, QPos1) ->
                   ?q({error, {?s(QError), ?s(QPos1)}})
           end).

%%
%% JSON parsers
%%
whitespace() ->   
    matches([?q($\s), ?q($\t), ?q($\r), ?q($\n)]).


nullable(Parser) ->
    mplus(
      right(match(?q("null")), return(?q(undefined))),
      Parser).


boolean() ->
    mplus(
      right(match(?q("true")), return(?q(true))),
      right(match(?q("false")), return(?q(false)))).

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

positive(Acc) ->
    bind(digit1_9(),
         fun(QD) ->
                 many_acc(digit(), ?q([?s(QD)|?s(Acc)]))
         end).

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

frac(Acc) ->
    bind(match(?q($.)),
         fun(_) ->
                 bind(digit(),
                      fun(QD) ->
                              many_acc(digit(), ?q([?s(QD),$.|?s(Acc)]))
                      end)
         end).

exp(Acc) ->    
    bind(e(Acc),
         fun(Acc1) ->
                 bind(digit(),
                      fun(D) ->
                              many_acc(digit(), ?q([?s(D)|?s(Acc1)]))
                      end)
         end).

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

float_digits(Acc) ->
    bind(int(Acc),
         fun(IDs) ->
                 bind(
                   option(frac(IDs), ?q([$0,$.|?s(IDs)])),
                   fun(FDs) ->
                           option(exp(FDs), FDs)
                   end)
         end).


integer() ->
    bind(int(?q([])),
         fun(QDs) ->
                 return(?q(list_to_integer(
                             lists:reverse(?s(QDs)))))
         end).

integer_p(Bin, Pos) ->
    ?s(inst_body(integer(), ?r(Bin), ?r(Pos))).


float() ->
    bind(float_digits(?q([])),
         fun(QDs) ->
                 return(?q(list_to_float(
                             lists:reverse(?s(QDs)))))
         end).

float_p(Bin, Pos) ->
    ?s(inst_body(float(), ?r(Bin), ?r(Pos))).


char() ->
    guard(fun(QC)->
                  ?q(?s(QC) =/= $"
                     andalso ?s(QC) =/= $\\
                     andalso ?s(QC) < 128)
          end).

string() ->
    bind(
      match(?q($")),
      fun(_) ->
              bind(
                many(mplus(
                       escape(),
                       substring())),
                fun(QSs) ->
                        bind(
                          match(?q($")),
                          fun(_) ->
                                  return(?q(list_to_binary(?s(QSs))))
                          end)
                end)
      end).

substring() ->
    bind(
      get_pos(),
      fun(P0) ->
              bind(
                skip_many1(char()),
                fun(_) ->
                        bind(
                          get_pos(),
                          fun(P1) ->
                                  bind(
                                    get_bin(),
                                    fun(QBin) ->
                                            return(
                                              ?q(binary:part(
                                                    ?s(QBin), ?s(P0),
                                                    ?s(P1)-?s(P0))))
                                    end)
                          end)
                end)
      end).

string_p(Bin, Pos) ->
    ?s(inst_body(string(), ?r(Bin), ?r(Pos))).


escape() ->
    bind(match(?q($\\)),
         fun(_) ->
                 mplus(
                   bind(match(?q($")),
                        fun(_) ->
                                return(?q(<<$">>))
                        end),
                   bind(match(?q($\\)),
                        fun(_) ->
                                return(?q(<<$\\>>))
                        end))
         end).

ws() ->
    skip_many(whitespace()).


object(FPNs) ->
    bind(
      match(?q(${)),
      fun(_) ->
              right(
                ws(),
                bind(
                  many(
                    left(object_field(FPNs),
                         object_field_delim())),
                  fun(Fs) ->
                          right(match(?q($})),
                                return(Fs))
                  end))
      end).

object_field_delim() ->
    right(ws(),
          left(option(match(?q($,)), ?q(no_comma)),
               ws())).


p_matches(SPs) ->
    fun(QBin, QPos, Success, Failure) ->
            QFs = [ ?q(fun(<<_:?s(QPos)/binary, $", ?s(S), $", _/binary>>) ->
                               Pos1 = ?s(QPos) + size(<<$",?s(S), $">>),
                               ?s(P(QBin,
                                    ?r(Pos1),
                                    fun(QVal, QPos1) ->
                                            ?q({ok, {?s(QVal), ?s(QPos1)}})
                                    end,
                                    fun(QErr, QPos2) ->
                                            ?q({error, {?s(QErr), ?s(QPos2)}})
                                    end))
                       end)
                    || {S,P} <- SPs ],
            QD = ?q(fun(_) -> {error, {none_matched, ?s(QPos)}} end),
            Fun = fun(Arg, Qs) ->
                          Cs = lists:flatmap(
                                 fun erl_syntax:fun_expr_clauses/1,
                                 Qs),            
                          ?v(?re(erl_syntax:case_expr(Arg, Cs)))
                  end,
            ?q(case ?s(Fun(?i(QBin), ?i(sequence(QFs ++ [QD])))) of
                   {ok, {_Val, _Pos1}} ->
                       ?s(Success(?r(_Val), ?r(_Pos1)));
                   {error, {_Err, _Pos2}} ->
                       ?s(Failure(?r(_Err), ?r(_Pos2)))
               end)
    end.
    

pair(F, P, N) ->
    right(
      ws(),
      right(
        match(?q($:)),
        right(
          ws(),
          fun(QBin, QPos, Success, Failure) ->
                  P(QBin, QPos,
                    fun(QVal, QPos1) ->
                            Success(?q({?s(N), ?s(QVal)}), QPos1)
                    end,
                    fun(QErr, QPos2) ->
                            Failure(?q({<<?s(F)>>, ?s(QErr)}), QPos2)
                    end)
          end))).

object_field(FPNs) ->
    SPs = [ {F, pair(F, P, N)} || {F, P, N} <- FPNs ],
    p_matches(SPs).

