-module(mparsers).

-include("monads.hrl").

-compile(export_all).

-meta(['>'/2, 'or'/2, oneof/1]).

-record(input, {bin, pos}).

-define(re(Syntax), erl_syntax:revert(Syntax)).


return(Value) ->
    fun(Inp) ->
            {ok, {Value, Inp}}
    end.

bind(Parser, Fun) ->
    fun(Inp) ->
            case Parser(Inp) of
                {ok, {Value, Inp1}} ->
                    (Fun(Value))(Inp1);
                {error, _} = Failure ->
                    Failure
            end
    end.

mzero() ->
    {error, mzero}.

mplus(Left, Right) ->
    fun(Inp) ->
        case Left(Inp) of
            {ok, _} = S ->
                S;
            {error, _} ->
                Right(Inp)
        end
    end.

            
get_pos() ->
    fun(#input{pos = Pos} = Inp) ->
            {ok, {Pos, Inp}}
    end.

get_bin() ->
    fun(#input{bin = Bin} = Inp) ->
            {ok, {Bin, Inp}}
    end.
    

'>'(Left, Right) ->
    ?q(bind(?s(?v(?e(Left))),
            fun(_) -> ?s(?v(?e(Right))) end)).

'or'(Left, Right) ->
    ?q(mplus(?s(?v(?e(Left))), ?s(?v(?e(Right))))).


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
    CQs = to_list(?e(QCs)),
    QFs = [ ?q(fun(#input{bin = <<?s(CQ), Rest/binary>>, pos = Pos}) ->
                      {ok, {?s(CQ), #input{bin = Rest, pos = Pos + 1}}}
               end)
           || CQ <- CQs],
    QCQs = sequence([?q(<<?s(Q)>>) || Q <- CQs]),
    QExp = ?v(?re(erl_syntax:list(?s(QCQs)))),
    QD = ?q(fun(Inp) ->
                    {error, {{expected, ?s(QExp)}, Inp}}
            end),
    QFs1 = ?v(lists:flatmap(fun erl_syntax:fun_expr_clauses/1,
                               ?s(sequence(QFs ++ [QD])))),
    ?v(?re(erl_syntax:fun_expr(?s(QFs1)))).

noneof(QCs) ->
    CQs = to_list(?e(QCs)),
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
    QFs1 = ?v(lists:flatmap(
                fun erl_syntax:fun_expr_clauses/1,
                ?s(sequence(QFs ++ [QD,QE])))),
    ?v(?re(erl_syntax:fun_expr(?s(QFs1)))).



option(Parser, Default) ->
    Parser or return(Default).


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

many_fold(Parser, Fun, Acc) ->
    fun(Inp) ->
            many_fold_iter(Parser, Inp, Fun, Acc)
    end.

many_fold_iter(Parser, Inp, Fun, Acc) ->                
    case Parser(Inp) of
        {ok, {Value, Inp1}} ->
            many_fold_iter(Parser, Inp1, Fun, Fun(Value,Acc));
        {error, _} ->
            {ok, {Acc, Inp}}
    end.

%% skip_many(Parser) ->
%%     fun(Inp) ->
%%             skip_many_iter(Parser, Inp)
%%     end.

%% skip_many_iter(Parser, Inp) ->                
%%     case Parser(Inp) of
%%         {ok, {_, Inp1}} ->
%%             skip_many_iter(Parser, Inp1);
%%         {error, _} ->
%%             {ok, {ok, Inp}}
%%     end.

sep1_by(Parser, Sep) ->
    do([ [X|Xs] ||
           X <- Parser,
           Xs <- many(Sep > Parser)]).

sep_by(Parser, Sep) ->
    sep1_by(Parser, Sep) or return([]).

    
%% %% %% %% sep1_by(Parser, Sep) ->
%% %% %% %%     fun(Inp) ->
%% %% %% %%             case Parser(Inp) of
%% %% %% %%                 {ok, {Value, Inp1}} ->
                    
%% %% %% %%             sep_by_iter(Parser, Sep, Inp, [])
%% %% %% %%     end.

%% %% %% %% sep_by_iter(Parser, Sep, Inp, Acc) ->
%% %% %% %%     case Parser(Inp) of
%% %% %% %%         {ok, {Value, Inp1}} ->
%% %% %% %%             case Sep(Inp1) of
%% %% %% %%                 {ok, {_, Inp2}} ->
%% %% %% %%                     sep_by_iter(Parser, Sep, Inp2, [Value|Acc]);
%% %% %% %%                 {error, _} ->
%% %% %% %%                      {ok, {lists:reverse([Value|Acc]), Inp1}}
%% %% %% %%             end;
%% %% %% %%         {error, _} ->
%% %% %% %%             {ok, {lists:reverse(Acc), Inp}}
%% %% %% %%     end.

%% %% %% sep_by(Parser, Sep) ->
%% %% %%     sep_by_1(Parser, Sep) or return([]).

%% %% %% sep_by_1(Parser, Sep) ->
%% %% %%     do([[X|Xs]
%% %% %%         || X <- Parser,
%% %% %%            Xs <- many(Sep > Parser)]).

%% t1(Parser, Sep) ->
%%     ?r(Parser) or ?r(Sep).

%% t2(Parser, Sep) ->
%%     ?r(Parser) > (?r(Parser) or ?r(Sep)).

%% %% t3(P1, P2) ->    
%% %%     ?s(tg(?qv(P1), ?qv(?s(tg(?qv(P1), ?qv(P2)))))).

%% %% t4(P1, P2) ->    
%% %%     tg2(P1, tg2(P1, P2)).

%% %% %% %%
%% %% %% %% JSON parsers
%% %% %% %%
%% whitespace() ->
%%     oneof(" \t\n\r").

%% ws() ->
%%     skip_many(whitespace()).

%% %% string() ->
%% %%     do([binary:part(B, 1, F-S-2)
%% %%         || B <- get_bin(),
%% %%            S <- get_pos(),
%% %%            skip_string(),
%% %%            F <- get_pos()]).

%% %% skip_string() ->
%% %%     do([ok ||
%% %%            singleton($"),
%% %%            many(string_char() or escape_seq()),
%% %%            singleton($")]).

%% string_char() ->
%%     noneof([$",$\\]).

%% escape_char() ->
%%     oneof("\"\\/bfnrt").
    
%% escape_seq() ->
%%     do([ok ||
%%            singleton($\\),
%%            escape_char()]).


%% %% sign() ->
%% %%     singleton($-) > return(-1).

%% %% zero() ->
%% %%     singleton($0) > return(0).

%% %% natural() ->
%% %%     do([list_to_integer([D|Ds])
%% %%         || D <- range($1,$9),
%% %%            Ds <- many(range($0,$9))]).

%% %% integer() ->    
%% %%       either(zero(),
%% %%             do([S * N 
%% %%                 || S <- option(sign(), 1),
%% %%                    N <- natural()])).

%% %% skip_integer() ->
%% %%     do([ok ||
%% %%            range($1, $9),
%% %%            many(range($0,$9))]).

%% %% integer_bin() ->
%% %%     do([binary:part(B, 0, F-S)
%% %%         || B <- get_bin(),
%% %%            S <- get_pos(),
%% %%            skip_integer(),
%% %%            F <- get_pos()]).

    
%% %% float() ->
%% %%     do([list_to_float(W ++ [$.|F])
%% %%         || W <- whole_part(),
%% %%            singleton($.),
%% %%            F <- fractional_part()]).

%% %% whole_part() ->
%% %%     (singleton($0) > return("0")) or
%% %%         do([[D|Ds] ||
%% %%                D <- range($1,$9),
%% %%                Ds <- many(range($0,$9))]).

%% %% fractional_part() ->
%% %%     do([[D|Ds]
%% %%         || D <- range($0,$9),
%% %%            Ds <- many(range($0,$9))]).


%% %% skip_float() ->
%% %%     do([ok
%% %%         || skip_whole(),
%% %%            singleton($.),
%% %%            skip_fractional()]).
           
%% %% skip_whole() ->        
%% %%     either(singleton($0),
%% %%            right(range($1,$9),skip_many(range($0,$9)))).

%% %% skip_fractional() ->        
%% %%     do([ok
%% %%        || range($0,$9),
%% %%           skip_many(range($0,$9))]).


%% %% boolean() ->
%% %%     fun(#input{bin = <<"true", Rest/binary>>, pos = Pos}) ->
%% %%             {ok, {true, #input{bin = Rest, pos = Pos + 4}}};
%% %%        (#input{bin = <<"false", Rest/binary>>, pos = Pos}) ->
%% %%             {ok, {false, #input{bin = Rest, pos = Pos + 5}}};
%% %%        (Inp) ->
%% %%             {error, {{expected, [<<"true">>, <<"false">>]}, Inp}}
%% %%     end.


%% %% %% array(Parser) ->
%% %% %%     do([Es ||
%% %% %%            singleton($[),
%% %% %%            ws(),
%% %% %%            Es <- sep_by(Parser, array_elem_sep()),
%% %% %% %%            ws(),
%% %% %% %%            singleton($])]).

%% %% %% %% array_elem_sep() ->
%% %% %% %%     do([ok || ws(), singleton($,), ws()]).


%% %% %% %% %% object() ->
%% %% %% %% %%     do([ok ||
%% %% %% %% %%            singleton(${),
%% %% %% %% %%            ws(),
%% %% %% %% %%            F <- string(),
%% %% %% %% %%            ws(),
%% %% %% %% %%            singleton]).

%% %% %% %% pair() ->
%% %% %% %%     do([{F,V} ||
%% %% %% %%            singleton()]).


test0() ->
    do([ A || A <- some:func1() ]).

test1() ->
    do([{A,B} ||
           A <- some:func1(),
           B <- some:func2()]).

test2() -> some:func1() > some:func1().

test3() ->
    do([{A,B} ||
           A <- some:func1(),
           B <- do([A1+B1 ||
                       {A1,_} <- some:func2(),
                       B1 <-some:func3()])]).

test4() ->
    do([{A,B} ||
           A <- some:func1() > some:func2(),
           B <- some:func3() or some:func4()]).

test5() ->
    oneof("abc").

%% test2() ->
%%     test0() or test().

%% %% test2() ->
%% %%     do2(?qv([{A,B} ||
%% %%                 A <- some:func1(),
%% %%                 B <- some:func2()])).


%%
%% Utils
%%

sequence([]) ->
    ?v([]);
sequence([Q|Qs]) ->
    ?v([?s(Q)|?s(sequence(Qs))]).

to_list({string, Ln, Ls}) ->
    [?v({char, Ln, C}) || C <- Ls];
to_list({cons, _, QE, Cs}) ->
    [?v(QE) | to_list(Cs)];
to_list({nil, _}) ->
    [].

