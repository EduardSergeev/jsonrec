-module(parsers).

-include_lib("meta/include/meta.hrl").

-export([return/1, bind/2, fail/1,
         mplus/2,
         left/2, right/2,
         get_bin/0,
         lift/1,
         match/1, matches/1,
         guard/1,
         many_acc/2,
         many/1, many1/1,
         sep_by_till_acc1/4,
         sep_by_till1/3, sep_by_till/3,
         skip_many/1, skip_many1/1,
         option/2,

         whitespace/0,
         null/0, nullable/1,
         boolean/0, integer/0, float/0, string/0,
         array/1,
         object/1,
         skip_json/0,
         any_json/0,

         ws_p/1, null_p/1,
         boolean_p/1, integer_p/1, float_p/1, string_p/1,
         skip_object_field_p/1, skip_json_p/1,
         any_json_p/1,

         to_parser/2,

         sequence/1]).

-define(re(Syntax), erl_syntax:revert(Syntax)).

%%
%% Basic monadic interface
%%
return(QVal) ->
    fun(QBin, Success, _Failure) ->  
            Success(QVal, QBin)
    end.

bind(Parser, Fun) ->
    fun(QBin, Success, Failure) ->
            Parser(QBin,
                   fun(QVal, QBin1) ->
                           Parser1 = Fun(QVal),
                           Parser1(QBin1, Success, Failure)
                   end,
                   Failure)
    end.

fail(QErr) ->
    fun(_QBin, _Success, Failure) ->  
            Failure(QErr)
    end.
    

%%
%% Our parser builder is also MonadPlus
%%
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
get_bin() ->
    fun(QBin, Success, _Failure) ->
            Success(QBin, QBin)
    end.

            
lift(QParser) ->
    fun(QBin, Success, Failure) ->
            ?q(case ?s(QParser)(?s(QBin)) of
                   {ok, {_Val, _Bin1}} ->
                       ?s(Success(?r(_Val), ?r(_Bin1)));
                   {error, _Err} ->
                       ?s(Failure(?r(_Err)))
               end)
    end.


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


guard(QExpFun) ->
    fun(QBin, Success, Failure) ->
            ?q(case ?s(QBin) of
                   <<C/utf8, Rest/binary>> when ?s(QExpFun(?r(C))) ->
                       ?s(Success(?r(C), ?r(Rest)));
                   _ ->
                       ?s(Failure(?q({does_not_satisfy, at, ?s(QBin)})))
               end)
    end.


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


many1(Parser) ->
    bind(Parser,
         fun(V) ->
                 bind(many(Parser),
                      fun(Vs) ->
                              return(?q([?s(V)|?s(Vs)]))
                      end)
         end).

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


skip_many1(Parser) ->
    bind(Parser,
         fun(_) ->
                 skip_many(Parser)
         end).


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
                                            ?q({ok, ?s(?r(Bin))})
                                    end))
                       end,
                   {_, _BinN} = Step(?s(QBin), Step),
                   ?s(Success(?q(ok), ?r(_BinN)))
               end)
    end.


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



sep_by_till(Parser, Sep, End) ->
    mplus(
      sep_by_till1(Parser, Sep, End),
      right(
        End,
        return(?q([])))).

sep_by_till1(Parser, Sep, End) ->
    bind(
      sep_by_till_acc1(Parser, Sep, End, ?q([])),
      fun(QVs) ->
              return(?q(lists:reverse(?s(QVs))))
      end).


sep_by_till_acc(Parser, Sep, End, QAcc) ->
    mplus(
      sep_by_till_acc1(Parser, Sep, End, QAcc),
      right(
        End,
        return(QAcc))).

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
%% Additional monadic functions
%%
sequence([]) ->
    ?v([]);
sequence([Q|Qs]) ->
    ?v([?s(Q)|?s(sequence(Qs))]).



%%
%% Utilify function for conversion of meta-parser to parser body (as a quote)
%%
to_parser(Parser, QBin) ->
    Parser(
      QBin,
      fun(QVal, QBin1) ->
              ?q({ok, {?s(QVal), ?s(QBin1)}})
      end,
      fun(QErr) ->
              ?q({error, ?s(QErr)})
      end).


%%
%% JSON parsers
%%
whitespace() ->   
    matches([?q($\s), ?q($\t), ?q($\r), ?q($\n)]).


null() ->
    right(match(?q("null")), return(?q(undefined))).

null_p(Bin) ->
    ?s(to_parser(null(), ?r(Bin))).

skip_null() ->
    right(null(), return(?q(ok))).

nullable(Parser) ->
    mplus(
      right(match(?q("null")), return(?q(undefined))),
      Parser).


boolean() ->
    mplus(
      right(match(?q("true")), return(?q(true))),
      right(match(?q("false")), return(?q(false)))).

skip_boolean() ->
    right(boolean(), return(?q(ok))).

boolean_p(Bin) ->
    ?s(to_parser(boolean(), ?r(Bin))).


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

digit_hex() ->
    guard(fun(QC) ->
                  ?q(?s(QC) >= $0 andalso ?s(QC) =< $9
                     orelse ?s(QC) >= $A andalso ?s(QC) =< $F
                     orelse ?s(QC) >= $a andalso ?s(QC) =< $f)
          end).

positive(Acc) ->
    bind(digit1_9(),
         fun(QD) ->
                 many_acc(digit(), ?q([?s(QD)|?s(Acc)]))
         end).

skip_positive() ->
    right(
      digit1_9(),
      skip_many(digit())).

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

skip_int() ->
    right(
      option(
        match(?q($-)),
        ?q(ok)),
      mplus(
        skip_positive(),
        digit0())).

frac(Acc) ->
    bind(match(?q($.)),
         fun(_) ->
                 bind(digit(),
                      fun(QD) ->
                              many_acc(digit(), ?q([?s(QD),$.|?s(Acc)]))
                      end)
         end).

skip_frac() ->
    right(
      match(?q($.)),
      right(
        digit(),
        skip_many(digit()))).

exp(Acc) ->    
    bind(e(Acc),
         fun(Acc1) ->
                 bind(digit(),
                      fun(D) ->
                              many_acc(digit(), ?q([?s(D)|?s(Acc1)]))
                      end)
         end).

skip_exp() ->    
    right(
      e(?q([])),
      right(
        digit(),
        skip_many(digit()))).

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

skip_float() ->
    right(
      skip_int(),
      right(
        option(skip_frac(), ?q(ok)),
        option(skip_exp(), ?q(ok)))).


integer() ->
    bind(int(?q([])),
         fun(QDs) ->
                 return(?q(list_to_integer(
                             lists:reverse(?s(QDs)))))
         end).

integer_p(Bin) ->
    ?s(to_parser(integer(), ?r(Bin))).


float() ->
    bind(float_digits(?q([])),
         fun(QDs) ->
                 return(?q(list_to_float(
                             lists:reverse(?s(QDs)))))
         end).

float_p(Bin) ->
    ?s(to_parser(float(), ?r(Bin))).


char() ->
    guard(fun(QC)->
                  ?q(?s(QC) =/= $" andalso ?s(QC) =/= $\\)
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
      get_bin(),
      fun(B0) ->
              bind(
                skip_many1(char()),
                fun(_) ->
                        bind(
                          get_bin(),
                          fun(B1) ->
                                  return(
                                    ?q(binary:part(
                                         ?s(B0), 0,
                                         byte_size(?s(B0))
                                         - byte_size(?s(B1)))))
                          end)
                end)
      end).

string_p(Bin) ->
    ?s(to_parser(string(), ?r(Bin))).


escape() ->
    right(
      match(?q($\\)),
      mplus(
        matches(
          [?q($"), ?q($\\), ?q($/), ?q($b), ?q($f), ?q($n), ?q($r), ?q($t)],
          [?q($"), ?q($\\), ?q($/), ?q($\b), ?q($\f), ?q($\n), ?q($\r), ?q($\t)]),
        right(
          match(?q($u)),
          uhex()))).

uhex() ->
    bind(
      count(?q(4), digit_hex()),
      fun(Cs) ->
              return(?q(<<(list_to_integer(?s(Cs), 16))/utf8>>))
      end).
    

ws() ->
    lift(?q(?MODULE:ws_p)).

ws_p(Inp) ->
    case Inp of
        <<$\s, Rest/binary>> ->
            ws_p(Rest);
        <<$\t, Rest/binary>> ->
            ws_p(Rest);
        <<$\r, Rest/binary>> ->
            ws_p(Rest);
        <<$\n, Rest/binary>> ->
            ws_p(Rest);
        _ ->
            {ok, {ok, Inp}}
    end.

comma_delim() ->
    right(ws(), left(match(?q($,)), ws())).


array(P) ->
    right(
      left(match(?q($[)), ws()),
      sep_by_till(
        P,
        comma_delim(),
        right(ws(), match(?q($]))))).


object(FPNs) ->
    right(
      left(match(?q(${)), ws()),
      bind(
        sep_by_till_acc(
          object_field(FPNs),
          comma_delim(),
          right(ws(), match(?q($}))),
          ?q([])),
        fun(Fs) ->
                return(
                  ?q(lists:filter(
                       fun is_tuple/1,
                       ?s(Fs))))
        end)).


p_matches(SPs) ->
    fun(QBin, Success, Failure) ->
            QFs = [ ?q(fun(<<$", ?s(S), $", Rest/binary>>) ->
                               ?s(P(?r(Rest),
                                    fun(QVal, QBin1) ->
                                            ?q({ok, {?s(QVal), ?s(QBin1)}})
                                    end,
                                    fun(QErr) ->
                                            ?q({error, ?s(QErr)})
                                    end))
                       end)
                    || {S,P} <- SPs ],
            QD = ?q(fun(_) ->
                            ?MODULE:skip_object_field_p(?s(QBin))
                    end),
            Fun = fun(Arg, Qs) ->
                          Cs = lists:flatmap(
                                 fun erl_syntax:fun_expr_clauses/1,
                                 Qs),            
                          ?v(?re(erl_syntax:case_expr(Arg, Cs)))
                  end,
            ?q(case ?s(Fun(?i(QBin), ?i(sequence(QFs ++ [QD])))) of
                   {ok, {_Val, _Bin1}} ->
                       ?s(Success(?r(_Val), ?r(_Bin1)));
                   {error, _Err} ->
                       ?s(Failure(?r(_Err)))
               end)
    end.
    

pair(F, P, N) ->
    right(
      ws(),
      right(
        match(?q($:)),
        right(
          ws(),
          fun(QBin, Success, Failure) ->
                  P(QBin,
                    fun(QVal, QBin1) ->
                            Success(?q({?s(N), ?s(QVal)}), QBin1)
                    end,
                    fun(QErr) ->
                            Failure(?q({<<?s(F)>>, ?s(QErr)}))
                    end)
          end))).

object_field(FPNs) ->
    SPs = [ {F, pair(F, P, N)} || {F, P, N} <- FPNs ],
    p_matches(SPs).


skip_object() ->
    right(
      left(match(?q(${)), ws()),
      sep_by_till_fold(
        skip_object_field(),
        comma_delim(),
        right(ws(), match(?q($}))),
        fun(_, _) -> ?q(ok) end,
        ?q(ok))).

skip_object_field() ->
    right(
      string(),
      right(
        ws(),
        right(
          match(?q($:)),
          right(
            ws(),
            lift(?q(?MODULE:skip_json_p)))))).

skip_object_field_p(Bin) ->
    ?s(to_parser(skip_object_field(), ?r(Bin))).


skip_json() ->
    mplus(
      skip_null(),
      mplus(
       skip_boolean(),
       mplus(
         skip_float(),
         mplus(
           right(string(), return(?q(ok))),
           mplus(
             right(
               array(lift(?q(?MODULE:skip_json_p))),
               return(?q(ok))),
             skip_object()))))).

skip_json_p(Bin) ->
    ?s(to_parser(skip_json(), ?r(Bin))).

any_json() ->
    bind(
      get_bin(),
      fun(B0) ->
              bind(
                right(
                  lift(?q(?MODULE:skip_json_p)),
                  get_bin()),
                fun(B1) ->
                        return(
                          ?q(binary:part(
                               ?s(B0), 0,
                               byte_size(?s(B0))
                               - byte_size(?s(B1)))))
                end)
      end).

any_json_p(Bin) ->
    ?s(to_parser(any_json(), ?r(Bin))).

