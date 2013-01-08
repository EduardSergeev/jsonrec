%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2012, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2012 by <eduard.sergeev@gmail.com>
%%%-------------------------------------------------------------------
-module(json_parsers).

-include("parsers.hrl").

-export([whitespace/0,
         null/0, nullable/1,
         boolean/0, integer/0, float/0, string/0,
         array/1,
         object/1,
         skip_json/0,
         any_json/0,

         ws_p/1, null_p/1,
         boolean_p/1, integer_p/1, float_p/1, string_p/1,
         skip_object_field_p/1, skip_json_p/1,
         any_json_p/1]).

-define(re(Syntax), erl_syntax:revert(Syntax)).

-spec whitespace() -> parsers:q_parser(ok).
whitespace() ->   
    matches([?q($\s), ?q($\t), ?q($\r), ?q($\n)]).


-spec null() -> parsers:q_parser(undefined).
null() ->
    right(match(?q("null")), return(?q(undefined))).

-spec null_p(parsers:input()) -> ?parser_result(undefined).
null_p(Bin) ->
    ?s(to_parser(null(), ?r(Bin))).

-spec skip_null() -> parsers:q_parser(ok).
skip_null() ->
    right(null(), return(?q(ok))).

-spec nullable(parsers:q_parser(Val)) ->
                      parsers:q_parser(Val|undefined) when
      Val :: any().   
nullable(Parser) ->
    mplus(
      right(match(?q("null")), return(?q(undefined))),
      Parser).

-spec boolean() -> parsers:q_parser(boolean()).
boolean() ->
    mplus(
      right(match(?q("true")), return(?q(true))),
      right(match(?q("false")), return(?q(false)))).

-spec skip_boolean() -> parsers:q_parser(ok).
skip_boolean() ->
    right(boolean(), return(?q(ok))).

-spec boolean_p(parsers:input()) -> ?parser_result(boolean()).
boolean_p(Bin) ->
    ?s(to_parser(boolean(), ?r(Bin))).


-type dec_digit() :: 16#30..16#39.
-type digit1_9() :: 16#31..16#39.
-type digit0() :: 16#30.
-type hex_digit() :: 16#30..16#39 | 16#41..16#46 | 16#61..16#66.

-spec digit() -> parsers:q_parser(dec_digit()).
digit() ->
    guard(fun(QC) ->
                  ?q(?s(QC) >= $0 andalso ?s(QC) =< $9)
          end).

-spec digit1_9() -> parsers:q_parser(digit1_9()).
digit1_9() ->
    guard(fun(QC) ->
                  ?q(?s(QC) >= $1 andalso ?s(QC) =< $9)
          end).

-spec digit0() -> parsers:q_parser(digit0()).
digit0() ->
    match(?q($0)).

-spec digit_hex() -> parsers:q_parser(hex_digit()).
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

-spec skip_int() -> parsers:q_parser(ok).
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

-spec integer() -> parsers:q_parser(integer()).
integer() ->
    bind(int(?q([])),
         fun(QDs) ->
                 return(?q(list_to_integer(
                             lists:reverse(?s(QDs)))))
         end).

-spec integer_p(parsers:input()) -> ?parser_result(integer()).
integer_p(Bin) ->
    ?s(to_parser(integer(), ?r(Bin))).


-spec float() -> parsers:q_parser(float()).
float() ->
    bind(float_digits(?q([])),
         fun(QDs) ->
                 return(?q(list_to_float(
                             lists:reverse(?s(QDs)))))
         end).

-spec float_p(parsers:input()) -> ?parser_result(float()).
float_p(Bin) ->
    ?s(to_parser(float(), ?r(Bin))).


char() ->
    guard(fun(QC)->
                  ?q(?s(QC) =/= $" andalso ?s(QC) =/= $\\)
          end).

-spec string() -> parsers:q_parser(binary()).
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


-spec string_p(parsers:input()) -> ?parser_result(binary()).
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
    
-spec ws() -> parsers:q_parser(ok).
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

-spec array(parsers:q_parser(Val)) -> parsers:q_parser([Val]) when
      Val :: any().
array(P) ->
    right(
      left(match(?q($[)), ws()),
      sep_by_till(
        P,
        comma_delim(),
        right(ws(), match(?q($]))))).


-spec object([{QField,QParser,QInd}]) -> parsers:q_parser([{Ind,Val}]) when
      QField :: meta:quote(atom()),
      QParser :: parsers:q_parser(Val),
      QInd :: meta:quote(Ind),
      Ind :: integer(),
      Val :: any().
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


-spec skip_object() -> parsers:q_parser(ok).
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


-spec skip_json() -> parsers:q_parser(ok).
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

-spec skip_json_p(parsers:input()) -> ?parser_result(ok).
skip_json_p(Bin) ->
    ?s(to_parser(skip_json(), ?r(Bin))).

-spec any_json() -> parsers:q_parser(Skipped) when
      Skipped :: binary().
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

-spec any_json_p(parsers:input()) -> ?parser_result(binary()).
any_json_p(Bin) ->
    ?s(to_parser(any_json(), ?r(Bin))).

