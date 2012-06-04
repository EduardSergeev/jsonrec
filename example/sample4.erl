-module(sample4).

-compile(export_all).
-compile({parse_transform, meta}).


s0() ->
    1.

s1() ->
    meta:quote(1).

s2(E) ->
    meta:quote(E+1).

s3() ->
    meta:splice(s1()).

s35() ->
    s2(meta:quote(1)).

s4(A) ->
    meta:splice(s2(A)).

s5() ->
    meta:quote(meta:splice({integer,0,1})).

s6() ->
    meta:quote(1+2).

%%s7() ->
%%    meta:reify(fun s2/1).
