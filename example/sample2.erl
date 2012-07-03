-module(sample2).

-include("../src/meta.hrl").

-record(rec,
        {id :: integer(),
         field1 :: binary()}).

-meta([do/1]).

-compile(export_all).

%% -export([expr0/0, expr/0, expr/1, expr/2]).
%% -export([t1/0, t2/0, t3/0]).

%%-export([s2/0, expr/1]).


do(Expr) ->
    meta:quote(meta:splice(Expr)+1).
    

e1() ->
    meta:quote(1).

e2() ->
    meta:quote(A+1).

%% e3(A) ->
%%     A + meta:splice(id(e2())).

e4() ->
    meta:quote(e1()).

id(X) ->
    X.

s1() ->
    {ap, 1,2}.

s2() ->
    1+2.

expr() ->
   meta:quote(2+3).


expr1() ->
    meta:quote((2+1)+1).

expr11() ->
    expr1().

expr(A) ->
    meta:quote(meta:splice(A)+1).

expr(A,B) ->
    meta:quote(meta:splice(A)+meta:splice(B)).

t1() ->
    A = meta:quote(2+1),
    expr(A).

%%t111() ->
%%    meta:splice(expr1(1)).

t11() ->
   meta:splice(id(t1())).

t2() ->
    fun(A) ->
	    meta:quote(meta:splice(A)+1)
    end.

t3() ->
    meta:quote(
      fun(A) ->
	      A+1
      end).

f1(A) ->
    A+1.

call(F, A) ->
    meta:quote((meta:splice(F))(meta:splice(A))).

test(Fu) ->
    F = fun(E, A) ->
                call(E, A)
        end,
    lists:foldr(F, meta:quote(1), lists:duplicate(3,Fu)).
    
ts() ->
    meta:splice(test(meta:quote(f1))).

fu2() ->
    (meta:splice(meta:quote(fun f1/1)))(1).

sa() ->
    meta:quote(f1(1)).

r1() ->
    meta:reify(fun s2/0).


r2() ->
    meta:reify(#rec{}).
