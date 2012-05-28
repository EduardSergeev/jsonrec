-module(splice).

-include("../src/meta.hrl").

-compile(export_all).

f() ->
    42.

f1(A) ->
    A + 1.

f2(B) ->
    B + 2.


mf() ->
    meta:reify(fun f/0).

mf(1) ->
    meta:reify(fun f1/1);
mf(_) ->
    meta:reify(fun f2/1).

mf(1,2) ->
    meta:reify(fun f2/1).

%%gg() ->
%%    meta:splice(mf(1)).

-splice(mf).
%%-splice({{sample2,r1},[]}).


