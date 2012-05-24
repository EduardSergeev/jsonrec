-module(sample2).

-include("../src/meta.hrl").

-record(rec,
        {id :: integer(),
         field1 :: binary()}).


-compile(export_all).

%% -export([expr0/0, expr/0, expr/1, expr/2]).
%% -export([t1/0, t2/0, t3/0]).

%%-export([s2/0, expr/1]).


e1() ->
    meta:quote(1).

e2() ->
    meta:quote(A+1).

e3(A) ->
    A + meta:splice((e2())).

e4() ->
    meta:quote(e1()).

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
    meta:quote(A+1).

expr(A,B) ->
    meta:quote(A+B).

t1() ->
    A = meta:quote(2+1),
    expr(A).

%%t111() ->
%%    meta:splice(expr1(1)).

t11() ->
    meta:splice(t1()).

t2() ->
    fun(A) ->
	    meta:quote(A+1)
    end.

t3() ->
    meta:quote(
      fun(A) ->
	      A+1
      end).

f1(A) ->
    A+1.

call(F, A) ->
    meta:quote(F(A)).

test(Fu) ->
    F = fun(E, A) ->
                call(E, A)
        end,
    lists:foldr(F, meta:quote(1), lists:duplicate(3,Fu)).
    
ts() ->
    meta:splice(test(meta:quote(f1))).


sa() ->
    meta:quote(f1(1)).

r1() ->
    meta:reify(fun s2/0).


r2() ->
    meta:reify(#rec{}).
