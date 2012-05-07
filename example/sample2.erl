-module(sample2).

-compile({parse_transform, meta}).

-export([expr0/0, expr/0, expr/1, expr/2]).

expr() ->
    meta:quote(2+3).


expr0() ->
    meta:quote(42).

expr(A) ->
    meta:quote(A+1).

expr(A,B) ->
    C = A + B,
    meta:quote(C+2).
