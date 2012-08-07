-module(monad_parser).

-include_lib("meta/include/meta.hrl").

%%-export([do/1, test/0]).

-compile(export_all).

-meta([do/1,
       'or'/2,
       '>'/2,
       oneof/1, noneof/1]).

-record(input, {bin, pos}).


do({lc, _Ln, Res, Exprs}) ->
    QInp = ?q(Inp),
    ?q(fun(?s(QInp)) ->
               ?s(expand(Exprs, QInp, 1, Res))
       end).

expand([], QInp, _N, Res) ->
    ?q({ok, {?s(Res), ?s(QInp)}});
expand([{generate, _Ln, Pat, Expr} | Rs], QInp, N, Res) ->
    QInp1 = gen_inp(N),
    ?q(case ?s(Expr)(?s(QInp)) of
           {ok, {?s(Pat), #input{} = ?s(QInp1)}} ->
               ?s(expand(Rs, QInp1, N + 1, Res)); 
           {error, _} = E ->
               E
       end);
expand([Expr | Rs], QInp, N, Res) ->
    QInp1 = gen_inp(N),
    ?q(case ?s(Expr)(?s(QInp)) of
           {ok, {_, #input{} = ?s(QInp1)}} ->
               ?s(expand(Rs, QInp1, N + 1, Res)); 
           {error, _} = E ->
               E
       end).
    

gen_inp(N) ->
    VarName = "Inp" ++ integer_to_list(N),
    AVar = list_to_atom(VarName),
    erl_syntax:revert(erl_syntax:variable(AVar)).

get_pos() ->
    fun(#input{pos = Pos} = Inp) ->
            {ok, {Pos, Inp}}
    end.

get_bin() ->
    fun(#input{bin = Bin} = Inp) ->
            {ok, {Bin, Inp}}
    end.
    


singleton(C) ->
    singleton(C, C).

singleton(C,V) ->
    fun(#input{bin = <<I, Rest/binary>>, pos = Pos})
          when I =:= C->
            {ok, {V, #input{bin = Rest, pos = Pos + 1}}};
       (Inp) ->
            {error, {{expected, <<C>>}, Inp}}
    end.

range(From, To) ->
    fun(#input{bin = <<C, Rest/binary>>, pos = Pos})
          when C >= From andalso C =< To ->
            {ok, {C, #input{bin = Rest, pos = Pos + 1}}};
       (Inp) ->
            {error, {{expected, {<<From>>,<<To>>}}, Inp}}
    end.


oneof(QCs) ->
    CQs = to_list(QCs),
    QFs = [ ?q(fun(#input{bin = <<?s(CQ), Rest/binary>>, pos = Pos}) ->
                      {ok, {?s(CQ), #input{bin = Rest, pos = Pos + 1}}}
              end)
           || CQ <- CQs],
    QExp = erl_syntax:revert(erl_syntax:list([?q(<<?s(Q)>>) || Q <- CQs])),
    QD = ?q(fun(Inp) ->
                    {error, {{expected, ?s(QExp)}, Inp}}
            end),
    QFs1 = lists:flatmap(fun erl_syntax:fun_expr_clauses/1, QFs ++ [QD]),
    erl_syntax:revert(erl_syntax:fun_expr(QFs1)).

noneof(QCs) ->
    CQs = to_list(QCs),
    QFs = [ ?q(fun(#input{bin = <<?s(CQ), _/binary>>} = Inp) ->
                      {error, {{unexpected, ?s(CQ)}, Inp}}
              end)
           || CQ <- CQs],
    QD = ?q(fun(#input{bin = <<C, Rest/binary>>, pos = Pos}) ->
                    {ok, {C, #input{bin = Rest, pos = Pos + 1}}}
            end),
    QE = ?q(fun(#input{bin = <<>>} = Inp) ->
                    {error, {eof, Inp}}
            end),
    QFs1 = lists:flatmap(fun erl_syntax:fun_expr_clauses/1, QFs ++ [QD,QE]),
    erl_syntax:revert(erl_syntax:fun_expr(QFs1)).


to_list({string, Ln, Ls}) ->
    [{char, Ln, C} || C <- Ls];
to_list({cons, _, QE, Cs}) ->
    [QE | to_list(Cs)];
to_list({nil, _}) ->
    [].


return(Value) ->
    fun(Inp) ->
            {ok, {Value, Inp}}
    end.

right(Left, Right) ->
    fun(Inp) ->
            case Left(Inp) of
                {ok, {_, Inp1}} ->
                    Right(Inp1);
                {error, _} = E ->
                    E
            end
    end.

'>'(Left, Right) ->
    ?q(fun(Inpr) ->
               case (?s(Left))(Inpr) of
                   {ok, {_, Inpr1}} ->
                       ?s(Right)(Inpr1);
                   {error, _} = E ->
                       E
               end
       end).

either(Left, Right) ->
    fun(Inp) ->
            case Left(Inp) of
                {ok, _} = Ok ->
                    Ok;
                {error, _} ->
                    Right(Inp)
            end
    end.

'or'(QLeft, QRight) ->
    ?q(fun(Inpo) ->
               case ?s(QLeft)(Inpo) of
                   {ok, _} = Ok ->
                       Ok;
                   {error, _} ->
                       ?s(QRight)(Inpo)
               end
       end).

option(Parser, Default) ->
    fun(Inp) ->
           case Parser(Inp) of
               {ok, _} = Ok ->
                   Ok;
               {error, _} ->
                   {ok, {Default, Inp}}
           end
    end.


many(Parser) ->
    fun(Inp) ->
            many_iter(Parser, Inp, [])
    end.

many_iter(Parser, Inp, Acc) ->                
    case Parser(Inp) of
        {ok, {Value, Inp1}} ->
            many_iter(Parser, Inp1, [Value|Acc]);
        {error, _} ->
            {ok, {lists:reverse(Acc), Inp}}
    end.

skip_many(Parser) ->
    fun(Inp) ->
            skip_many_iter(Parser, Inp)
    end.

skip_many_iter(Parser, Inp) ->                
    case Parser(Inp) of
        {ok, {_, Inp1}} ->
            skip_many_iter(Parser, Inp1);
        {error, _} ->
            {ok, {ok, Inp}}
    end.
    
%% sep1_by(Parser, Sep) ->
%%     fun(Inp) ->
%%             case Parser(Inp) of
%%                 {ok, {Value, Inp1}} ->
                    
%%             sep_by_iter(Parser, Sep, Inp, [])
%%     end.

%% sep_by_iter(Parser, Sep, Inp, Acc) ->
%%     case Parser(Inp) of
%%         {ok, {Value, Inp1}} ->
%%             case Sep(Inp1) of
%%                 {ok, {_, Inp2}} ->
%%                     sep_by_iter(Parser, Sep, Inp2, [Value|Acc]);
%%                 {error, _} ->
%%                      {ok, {lists:reverse([Value|Acc]), Inp1}}
%%             end;
%%         {error, _} ->
%%             {ok, {lists:reverse(Acc), Inp}}
%%     end.

sep_by(Parser, Sep) ->
    sep_by_1(Parser, Sep) or return([]).

sep_by_1(Parser, Sep) ->
    do([[X|Xs]
        || X <- Parser,
           Xs <- many(Sep > Parser)]).

t1(Parser, Sep) ->
    Parser or Sep.

t2(Parser, Sep) ->
    Parser > (Parser or Sep).
    

%%
%% JSON parsers
%%
whitespace() ->
    oneof(" \t\n\r").

ws() ->
    skip_many(whitespace()).

string() ->
    do([binary:part(B, 1, F-S-2)
        || B <- get_bin(),
           S <- get_pos(),
           skip_string(),
           F <- get_pos()]).

skip_string() ->
    do([ok ||
           singleton($"),
           many(string_char() or escape_seq()),
           singleton($")]).

string_char() ->
    noneof([$",$\\]).

escape_char() ->
    oneof("\"\\/bfnrt").
    
escape_seq() ->
    do([ok ||
           singleton($\\),
           escape_char()]).

%% escape_to_char($") ->
%%     $";
%% escape_to_char($\\) ->
%%     $\\;
%% escape_to_char($/) ->
%%     $/;
%% escape_to_char($b) ->
%%     $\b;
%% escape_to_char($f) ->
%%     $\f;
%% escape_to_char($n) ->
%%     $\n;
%% escape_to_char($r) ->
%%     $\r;
%% escape_to_char($t) ->
%%     $\t.


sign() ->
    singleton($-) > return(-1).

zero() ->
    singleton($0) > return(0).

natural() ->
    do([list_to_integer([D|Ds])
        || D <- range($1,$9),
           Ds <- many(range($0,$9))]).

integer() ->    
      either(zero(),
            do([S * N 
                || S <- option(sign(), 1),
                   N <- natural()])).

skip_integer() ->
    do([ok ||
           range($1, $9),
           many(range($0,$9))]).

integer_bin() ->
    do([binary:part(B, 0, F-S)
        || B <- get_bin(),
           S <- get_pos(),
           skip_integer(),
           F <- get_pos()]).

    
float() ->
    do([list_to_float(W ++ [$.|F])
        || W <- whole_part(),
           singleton($.),
           F <- fractional_part()]).

whole_part() ->
    (singleton($0) > return("0")) or
        do([[D|Ds] ||
               D <- range($1,$9),
               Ds <- many(range($0,$9))]).

fractional_part() ->
    do([[D|Ds]
        || D <- range($0,$9),
           Ds <- many(range($0,$9))]).


skip_float() ->
    do([ok
        || skip_whole(),
           singleton($.),
           skip_fractional()]).
           
skip_whole() ->        
    either(singleton($0),
           right(range($1,$9),skip_many(range($0,$9)))).

skip_fractional() ->        
    do([ok
       || range($0,$9),
          skip_many(range($0,$9))]).


boolean() ->
    fun(#input{bin = <<"true", Rest/binary>>, pos = Pos}) ->
            {ok, {true, #input{bin = Rest, pos = Pos + 4}}};
       (#input{bin = <<"false", Rest/binary>>, pos = Pos}) ->
            {ok, {false, #input{bin = Rest, pos = Pos + 5}}};
       (Inp) ->
            {error, {{expected, [<<"true">>, <<"false">>]}, Inp}}
    end.


array(Parser) ->
    do([Es ||
           singleton($[),
           ws(),
           Es <- sep_by(Parser, array_elem_sep()),
           ws(),
           singleton($])]).

array_elem_sep() ->
    do([ok || ws(), singleton($,), ws()]).


%% object() ->
%%     do([ok ||
%%            singleton(${),
%%            ws(),
%%            F <- string(),
%%            ws(),
%%            singleton]).

%% pair() ->
%%     do([{F,V} ||
%%        singleton($)]).
