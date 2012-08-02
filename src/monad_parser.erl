-module(monad_parser).

-include_lib("meta/include/meta.hrl").

%%-export([do/1, test/0]).

-compile(export_all).

-meta([do/1,
       return/1,
       '>'/2,
       singleton/1,
       many/1,
       oneof/1]).

-record(input, {bin, pos}).

%% -define(REMOTE_CALL(Ln, Mod, Name, Args),
%%         #call{line = Ln,
%%               function = #remote
%%               {module = #atom{name = Mod},
%%                name = #atom{name = Name}},
%%               args = Args}).
%% -define(LOCAL_CALL(Ln, Name, Args),
%%         #call{line = Ln,
%%               function = #atom{name = Name},
%%               args = Args}).


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
    


singleton(V) ->
    ?q(fun(#input{bin = <<?s(V), Rest/binary>>, pos = Pos}) ->
               {ok, {?s(V), #input{bin = Rest, pos = Pos + 1}}};
          (_) ->
               {error, {expected, <<?s(V)>>}}
       end).

range(From, To) ->
    fun(#input{bin = <<C, Rest/binary>>, pos = Pos})
          when C >= From andalso C =< To ->
            {ok, {C, #input{bin = Rest, pos = Pos + 1}}};
       (_) ->
            {error, {expected, {<<From>>,<<To>>}}}
    end.

oneof(QCs) ->
    CQs = to_list(QCs),
    QFs = [ ?q(fun(#input{bin = <<?s(CQ), Rest/binary>>, pos = Pos}) ->
                      {ok, {?s(CQ), #input{bin = Rest, pos = Pos + 1}}}
              end)
           || CQ <- CQs],
    QExp = erl_syntax:revert(erl_syntax:list([?q(<<?s(Q)>>) || Q <- CQs])),
    QD = ?q(fun(_) ->
                    {error, {expected, ?s(QExp)}}
            end),
    QFs1 = lists:flatmap(fun erl_syntax:fun_expr_clauses/1, QFs ++ [QD]),
    erl_syntax:revert(erl_syntax:fun_expr(QFs1)).

to_list({cons, _, QE, Cs}) ->
    [QE | to_list(Cs)];
to_list({nil, _}) ->
    [].


return(QValue) ->
    ?q(fun(Inp) ->
               {ok, {?s(QValue), Inp}}
       end).

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
    ?q(fun(ManyInp) ->
               Iter = ?s(many_iter(Parser)),
               Iter(Iter, ManyInp, [])
       end).

many_iter(Parser) ->
    ?q(fun(Cont, ManyIterInp, Acc) ->
               case ?s(Parser)(ManyIterInp) of
                   {ok, {Value, ManyIterInp1}} ->
                       Cont(Cont, ManyIterInp1, [Value|Acc]);
                   {error, _} ->
                       {ok, {lists:reverse(Acc), ManyIterInp}}
               end
       end).


%%
%% JSON parsers
%%

aob() ->
     oneof([$a,$b]).

%% escape_char() ->
%%     do([
%%        singleton($\)])

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
    either(
      zero(),
      do([S * N 
          || S <- option(sign(), 1),
             N <- natural()])).

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
        
