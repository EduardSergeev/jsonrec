-module(parsers).

-include_lib("meta/include/meta.hrl").

-export([return/1, bind/2, fail/1,
         mplus/2,
         left/2, right/2,
         get_bin/0,
         lift/1,
         match/1, matches/1,
         guard/1,
         fold/3, fold_iter/4,
         many_acc/2, %% many_acc_iter/3,
         many/1, many1/1,
         skip_many/1, skip_many1/1, %% skip_many_iter/2,
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

         inst_body/2, to_parser/2,

         sequence/1]).

-define(re(Syntax), erl_syntax:revert(Syntax)).

%% -compile(bin_opt_info).
-compile(export_all).

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

%% bind(Parser, Fun) ->
%%     fun(QBin, Success, Failure) ->
%%             ?q(case ?s(Parser(
%%                          QBin,
%%                          fun(QVal, QBin1) ->
%%                                  Parser1 = Fun(QVal),
%%                                  Parser1(
%%                                    QBin1,
%%                                    Success,
%%                                    fun(QErr, QBin2) ->
%%                                            ?q({error, {?s(QErr), ?s(QBin2)}})
%%                                    end)
%%                          end,
%%                          fun(QErr, QBin2) ->
%%                                  ?q({error, {?s(QErr), ?s(QBin2)}})
%%                          end)) of
%%                    {error, {_Err, _Bin}} ->
%%                        ?s(Failure(?r(_Err), ?r(_Bin)));
%%                    OK ->
%%                        OK
%%                end)
%%     end.



fail(QErr) ->
    fun(QBin, _Success, Failure) ->  
            Failure(QErr, QBin)
    end.
    

%%
%% Our parser builder is also MonadPlus
%%
mplus(Left, Right) ->
    fun(QBin, Success, Failure) ->
            ?q(case ?s(Left(QBin,
                            fun(QVal1, QBin1) ->
                                    ?q({ok, {?s(QVal1), ?s(QBin1)}})
                            end,
                            fun(_QErr1, _QBin1) ->
                                    Right(QBin,
                                          fun(QVal2, QBin2) ->
                                                  ?q({ok, {?s(QVal2), ?s(QBin2)}})
                                          end,
                                          Failure)
                            end)) of
                   {ok, {_Val, _Bin}} ->
                       ?s(Success(?r(_Val), ?r(_Bin)));
                   Err ->
                       Err
               end)
    end.
%% mplus(Left, Right) ->
%%     fun(QBin, Success, Failure) ->
%%             Left(QBin,
%%                  Success,
%%                  fun(_QErr, QBin1) ->
%%                          Right(QBin1, Success, Failure)
%%                  end)
%%     end.
%% mplus(Left, Right) ->
%%     fun(QBin, Success, Failure) ->
%%             ?q(begin
%%                    Succ = fun(_Val, _Bin) ->
%%                                   ?s(Success(?r(_Val), ?r(_Bin)))
%%                           end,
%%                    ?s(Left(QBin,
%%                            fun(QVal1, QBin1) ->
%%                                    ?q(?s(?r(Succ))(?s(QVal1), ?s(QBin1)))
%%                            end,
%%                            fun(_QErr1, _QBin1) ->
%%                                    Right(QBin,
%%                                          fun(QVal2, QBin2) ->
%%                                                  ?q(?s(?r(Succ))(?s(QVal2), ?s(QBin2)))
%%                                          end,
%%                                          Failure)
%%                            end))
%%                end)
%%     end.

    

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
                   {error, {_Err, _Bin2}} ->
                       ?s(Failure(?r(_Err), ?r(_Bin2)))
               end)
    end.


match(QC) ->
    fun(QBin, Success, Failure) ->
            ?q(case ?s(QBin) of
                   <<?s(QC), Rest/binary>> ->
                       ?s(Success(QC, ?r(Rest)));
                   _ ->
                       ?s(Failure(?q({expected, <<?s(QC)>>}), QBin))
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
                       ?s(Failure(?q(unexpected), QBin))
               end)
    end.


guard(QExpFun) ->
    fun(QBin, Success, Failure) ->
            ?q(case ?s(QBin) of
                   <<C/utf8, Rest/binary>> when ?s(QExpFun(?r(C))) ->
                       ?s(Success(?r(C), ?r(Rest)));
                   _ ->
                       ?s(Failure(?q(does_not_satisfy), QBin))
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
                                    fun(_QErr, _QBin2) ->
                                            ?q({lists:reverse(?s(?r(Acc))),
                                                ?s(?r(Bin))})
                                    end))
                       end,
                   {_Vals, _BinN} = Step(?s(QBin), Step, []),
                   ?s(Success(?r(_Vals), ?r(_BinN)))
               end)
    end.

%% many(Parser) ->
%%     fun(QBin, Success, _) ->
%%             QParser = ?q(fun(Bin) ->
%%                                  ?s(Parser(?r(Bin),
%%                                            fun(QVal, QBin1) ->
%%                                                    ?q({ok, {?s(QVal), ?s(QBin1)}})
%%                                            end,
%%                                            fun(QErr, QBin2) ->
%%                                                    ?q({error, {?s(QErr), ?s(QBin2)}})
%%                                            end))
%%                          end),
%%             ?q(begin
%%                    {_Vals, Pos1} = ?MODULE:many_iter(?s(QParser), ?s(QBin), []),
%%                    ?s(Success(?r(_Vals), ?r(Pos1)))
%%                end)
%%     end.


%% many_iter(Parser, Bin, Acc) ->
%%     case Parser(Bin) of
%%         {ok, {Val, Bin1}} ->
%%             many_iter(Parser, Bin1, [Val|Acc]);
%%         {error, _} ->
%%             {lists:reverse(Acc), Bin}
%%     end.

many1(Parser) ->
    bind(Parser,
         fun(V) ->
                 bind(many(Parser),
                      fun(Vs) ->
                              return(?q([?s(V)|?s(Vs)]))
                      end)
         end).

fold(Parser, QFun, QAcc) ->
    fun(QBin, Success, _) ->
            QParser =
                ?q(fun(Bin) ->
                           ?s(Parser(
                                ?r(Bin),
                                fun(QVal, QBin1) ->
                                        ?q({ok, {?s(QVal), ?s(QBin1)}})
                                end,
                                fun(QErr, QBin2) ->
                                        ?q({error, {?s(QErr), ?s(QBin2)}})
                                end))
                         end),
            ?q(begin
                   {_Vals, _BinN} = 
                       ?MODULE:fold_iter(
                          ?s(QParser), ?s(QBin),
                          ?s(QFun), ?s(QAcc)),
                   ?s(Success(?r(_Vals), ?r(_BinN)))
               end)
    end.
    
fold_iter(Parser, Bin, Fun, Acc) ->
    case Parser(Bin) of
        {ok, {Val, Bin1}} ->
            fold_iter(Parser, Bin1, Fun, Fun(Val, Acc));
        {error, _} ->
            {Acc, Bin}
    end.


%% many_acc(Parser, QAcc) ->
%%     fun(QBin, Success, _) ->
%%             QParser =
%%                 ?q(fun(Bin) ->
%%                            ?s(Parser(
%%                                 ?r(Bin),
%%                                 fun(QVal, QBin1) ->
%%                                         ?q({ok, {?s(QVal), ?s(QBin1)}})
%%                                 end,
%%                                 fun(QErr, QBin2) ->
%%                                         ?q({error, {?s(QErr), ?s(QBin2)}})
%%                                 end))
%%                    end),
%%             ?q(begin
%%                    {_Vals, _BinN} =
%%                        ?MODULE:many_acc_iter(
%%                           ?s(QParser), ?s(QBin), ?s(QAcc)),
%%                    ?s(Success(?r(_Vals), ?r(_BinN)))
%%                end)
%%     end.

%% many_acc_iter(Parser, Bin, Acc) ->
%%     case Parser(Bin) of
%%         {ok, {Val, Bin1}} ->
%%             many_acc_iter(Parser, Bin1, [Val|Acc]);
%%         {error, _} ->
%%             {Acc, Bin}
%%     end.

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
                                    fun(_QErr, _QBin2) ->
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

%% skip_many(Parser) ->
%%     fun(QBin, Success, _) ->
%%             QStep =
%%                 ?q(fun(Bin) ->
%%                            ?s(Parser(
%%                                 ?r(Bin),
%%                                 fun(_QVal, QBin1) ->
%%                                         ?q({ok, {ok, ?s(QBin1)}})
%%                                 end,
%%                                 fun(_QErr, _QBin2) ->
%%                                         ?q({error, {stop, stop}})
%%                                 end))
%%                    end),
%%             Success(
%%               ?q(ok),
%%               ?q(?MODULE:skip_many_iter(?s(QStep), ?s(QBin))))
%%     end.

%% skip_many_iter(ParserFun, Bin) ->
%%     case ParserFun(Bin) of
%%         {ok, {_, Bin1}} ->
%%             skip_many_iter(ParserFun, Bin1);
%%         {error, _} ->
%%             Bin
%%     end.

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
                                    fun(_QErr, _QBin2) ->
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
                                    fun(QErr, QBin2) ->
                                            Failure(QErr, QBin2)
                                    end))
                       end,
                   Step(?s(QBin), Step, [], ?s(QN))
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
inst_body(Parser, QBin) ->
    Parser(
      QBin,
      fun(QVal, QBin1) ->
              ?q({ok,
                  {?s(QVal),
                   byte_size(?s(QBin)) - byte_size(?s(QBin1))}})
      end,
      fun(QErr, QBin2) ->
              ?q({error,
                  {?s(QErr),
                   byte_size(?s(QBin)) - byte_size(?s(QBin2))}})
      end).

to_parser(Parser, QBin) ->
    Parser(
      QBin,
      fun(QVal, QBin1) ->
              ?q({ok, {?s(QVal), ?s(QBin1)}})
      end,
      fun(QErr, QBin2) ->
              ?q({error, {?s(QErr), ?s(QBin2)}})
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


nullable(Parser) ->
    mplus(
      right(match(?q("null")), return(?q(undefined))),
      Parser).


boolean() ->
    mplus(
      right(match(?q("true")), return(?q(true))),
      right(match(?q("false")), return(?q(false)))).

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
        ?q(undefined)),
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
        option(skip_frac(), ?q(undefined)),
        option(skip_exp(), ?q(undefined)))).


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
    %% skip_many(whitespace()).

%% ws_p(Bin) ->
%%     ?s(to_parser(
%%          skip_many(whitespace()),
%%          ?r(Bin))).

%% ws_p(<<$\s, Rest/binary>>) ->
%%     ws_p(Rest);
%% ws_p(<<$\t, Rest/binary>>) ->
%%     ws_p(Rest);
%% ws_p(<<$\r, Rest/binary>>) ->
%%     ws_p(Rest);
%% ws_p(<<$\n, Rest/binary>>) ->
%%     ws_p(Rest);
%% ws_p(Bin) ->
%%     {ok, {ok, Bin}}.

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
    right(ws(),
          left(option(match(?q($,)), ?q(no_comma)),
               ws())).


array(P) ->
    left(
      right(
        left(match(?q($[)), ws()),
        many(left(P, comma_delim()))),
      match(?q($]))).


object(FPNs) ->
    bind(
      match(?q(${)),
      fun(_) ->
              right(
                ws(),
                bind(
                  many(
                    left(object_field(FPNs),
                         comma_delim())),
                  fun(Fs) ->
                          right(match(?q($})),
                                return(
                                  ?q(lists:filter(
                                       fun is_tuple/1,
                                       ?s(Fs)))))
                  end))
      end).

skip_object() ->
    bind(
      match(?q(${)),
      fun(_) ->
              right(
                ws(),
                bind(
                  skip_many(
                    left(skip_object_field(),
                         comma_delim())),
                  fun(_) ->
                          right(match(?q($})),
                                return(?q(ok)))
                  end))
      end).

p_matches(SPs) ->
    fun(QBin, Success, Failure) ->
            QFs = [ ?q(fun(<<$", ?s(S), $", Rest/binary>>) ->
                               ?s(P(?r(Rest),
                                    fun(QVal, QBin1) ->
                                            ?q({ok, {?s(QVal), ?s(QBin1)}})
                                    end,
                                    fun(QErr, QBin2) ->
                                            ?q({error, {?s(QErr), ?s(QBin2)}})
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
                   {error, {_Err, _Bin2}} ->
                       ?s(Failure(?r(_Err), ?r(_Bin2)))
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
                    fun(QErr, QBin2) ->
                            Failure(?q({<<?s(F)>>, ?s(QErr)}), QBin2)
                    end)
          end))).

object_field(FPNs) ->
    SPs = [ {F, pair(F, P, N)} || {F, P, N} <- FPNs ],
    p_matches(SPs).

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
      null(),
      mplus(
       boolean(),
       mplus(
         skip_float(),
         mplus(
           string(),
           mplus(
             array(lift(?q(?MODULE:skip_json_p))),
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


tt(Inp) ->
    ?s(to_parser(uhex(),
                 ?r(Inp))).

tt2(Inp) ->
    ?s(to_parser(escape(),
                 ?r(Inp))).
