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
    ?q(fun(Inp) ->
               case (?s(Left))(Inp) of
                   {ok, {_, Inp1}} ->
                       ?s(Right)(Inp1);
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
    ?q(fun(Inp) ->
               case ?s(QLeft)(Inp) of
                   {ok, _} = Ok ->
                       Ok;
                   {error, _} ->
                       ?s(QRight)(Inp)
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

%%
%% JSON parsers
%%

aob() ->
     oneof("ab").

string() ->
    do([binary:part(B, 0, F-S)
        || singleton($"),
           B <- get_bin(),
           S <- get_pos(),
           many(either(string_char(), escape_seq())),
           F <- get_pos(),
           singleton($")]).

string_char() ->
    noneof([$",$\\]).

escape_char() ->
    oneof("\"\\/bfnrt").
    
escape_seq() ->
    do([ok
        || singleton($\\),
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




%% string_char() ->
%%     (singleton($2)) > natural().

%% string() ->
%%     do(
%%      || singleton($"))


%% test() ->
%%      ?q(singleton($0)).

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
    either(
      right(singleton($0), return("0")),
      do([[D|Ds]
          || D <- range($1,$9),
             Ds <- many(range($0,$9))])).

fractional_part() ->
    do([[D|Ds]
        || D <- range($0,$9),
           Ds <- many(range($0,$9))]).


        
