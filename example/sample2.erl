-module(sample2).

-compile({parse_transform, meta}).

-compile(export_all).

%% -export([expr0/0, expr/0, expr/1, expr/2]).
%% -export([t1/0, t2/0, t3/0]).

%%-export([s2/0, expr/1]).


%% s1() ->
%%     {ap, 1,2}.

s2() ->
    1+2.

expr() ->
   meta:quote(2+3).


expr1() ->
    meta:quote(42+1).

expr11() ->
    expr1().

expr(A) ->
    meta:quote(A+1).

expr(A,B) ->
    meta:quote(A+B).

t1() ->
    A = meta:quote(2+1),
    expr(A).

t11() ->
    meta:splice(expr11()).

t2() ->
    fun(A) ->
	    meta:quote(A+1)
    end.

t3() ->
    meta:quote(
      fun(A) ->
	      A+1
      end).
