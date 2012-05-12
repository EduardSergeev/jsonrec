-module(sample2).

-compile({parse_transform, meta}).

-export([expr0/0, expr/0, expr/1, expr/2]).
-export([t1/0, t2/0, t3/0]).

%%-export([expr/0]).

expr() ->
    meta:quote(2+3).


expr0() ->
    meta:quote(42).

expr(A) ->
    meta:quote(A+1).

expr(A,B) ->
    C = A + B,
    meta:quote(C+2).

t1() ->
    A = meta:quote(2+1),
    F = fun(O) ->
		meta:quote(meta:splice(A)+O)
	end,
    F.

t2() ->
    fun(A) ->
	    meta:quote(A+1)
    end.

t3() ->
    meta:quote(
      fun(A) ->
	      A+1
      end).
