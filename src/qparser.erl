-module(qparser).

-include_lib("meta/include/meta.hrl").

-compile(export_all).
-meta([return/1,
       bind/2,
       do/1
]).

%% -meta([do/1,
%%        'or'/2,
%%        '>'/2,
%%        tg2/2,
%%        oneof/1, noneof/1]).

%%-record(input, {bin :: binary(), pos :: integer()}).
%%-type result() :: {ok, any(), #input{}} | {error, any()}.

%%-type parser() :: fun((#input{}) -> result()).
                       

-define(re(Syntax), erl_syntax:revert(Syntax)).

%%-spec return(any()) ->
return(QValue) ->
    ?q(fun(Inp) ->
               {ok, ?s(QValue), Inp}
       end).

bind(Parser, Fun) ->
    ?q(fun(Inp) ->
               case ?s(Parser)(Inp) of
                   {ok, Value, Inp1} ->
                       ((?s(Fun))(Value))(Inp1);
                   {error, _} = Error ->
                       Error
               end
       end).

%% to_parser(Exp) ->
%%     ?q(fun(Inp) ->
%%                ?s(Exp(?r(Inp)))
%%        end).
                           
test() ->            
    return(begin Inp = 42, Inp end).

%% f1() ->
%%      ?q(fun(V) -> ?s(return(?r(V))) end).


test2() ->            
    bind(return(begin Inp = 42, Inp end),
         fun(Inp) -> return(?r(Inp)) end).
                      
%% test3() ->            
%%     do([ X+Y ||
%%             X <- return(42),
%%             Y <- return(1) ]).

test4() ->            
     do([ X || X <- return(42) ]).


do(QLC) ->
    {lc, _Ln, Res, Exprs} = ?e(QLC),
    ?q(fun(Inp) ->
               ?s(expand(Exprs, ?r(Inp), ?v(Res)))
       end).


expand([], QInp, Res) ->
    ?q({ok, {?s(Res), ?s(QInp)}});
expand([{generate, _Ln, Pat, Expr} | Rs], QInp, Res) ->
    ?q(case ?s(?v(Expr))(?s(QInp)) of
           {ok, {?s(?v(Pat)), #input{} = Inp}} ->
               ?s(expand(Rs, ?r(Inp), Res)); 
           {error, _} = E ->
               E
       end);
expand([Expr | Rs], QInp, Res) ->
    ?q(case ?s(?v(Expr))(?s(QInp)) of
           {ok, {_, #input{} = Inp}} ->
               ?s(expand(Rs, ?r(Inp), Res)); 
           {error, _} = E ->
               E
       end).


%% test() ->
%%     fun (Inp) ->
%%             case (func1())(Inp) of
%%               {ok, {A, #input{} = Inp1}} ->
%%                   case (func2())(Inp1) of
%%                     {ok, {B, #input{} = Inp2}} -> {ok, {{A, B}, Inp2}};
%%                     {error, _} = E1 -> E1
%%                   end;
%%               {error, _} = E -> E
%%             end
%%     end.

%% func1() ->
%%     fun(Inp) ->
%%             {ok, {1, Inp}}
%%     end.

%% func2() ->
%%     fun(#input{bin = <<C, Rest/binary>>, pos = Pos}) ->
%%             {ok, {C, #input{bin = Rest, pos = Pos + 1}}}
%%     end.

%% test2() ->
%%     fun (Inp) ->
%%             case (fun(Inp2) -> {ok, {1, Inp2}} end)(Inp) of
%%               {ok, {A, #input{} = Inp1}} ->
%%                   case (fun(#input{bin = <<C, Rest/binary>>, pos = Pos}) ->
%%                                 {ok, {C, #input{bin = Rest, pos = Pos + 1}}}
%%                         end)(Inp1) of
%%                       {ok, {B, #input{} = Inp2}} -> {ok, {{A, B}, Inp2}};
%%                       {error, _} = E1 -> E1
%%                   end;
%%                 {error, _} = E -> E
%%             end
%%     end.
