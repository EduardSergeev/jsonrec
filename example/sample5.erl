-module(sample5).

-compile(export_all).
-compile({parse_transform, meta}).


%% e0() ->
%%     A = {integer,0,5},
%%     meta:splice(A).

e01(A) ->
    meta:quote(meta:splice(A)+1).

e1() ->
    A = 5,
    meta:quote(meta:splice(A)).

e2() ->
    A = 5,
    meta:splice(meta:quote(A)).

e21(A) ->
    meta:quote(meta:splice(A)).

%% e3() ->
%%     A = 5,
%%     meta:splice(A).

e31() ->
    A = meta:quote(5),
    meta:quote(meta:splice(A)).

%% e32() ->
%%     A = meta:quote(5),
%%     meta:splice(meta:quote(meta:splice(A))).

%% e33() ->
%%     meta:splice(
%%       A = meta:quote(5),
%%       A).


e4(A) ->
    meta:splice(meta:quote(A)).

e5_m(MRec) ->
    meta:quote(1 + meta:splice(MRec)).

e5_m1(MRec) ->
    MRec1 = rr:something(MRec),
    meta:quote(1 + meta:splice(MRec1)).

e5(_Rec) ->
    e5_m(meta:quote(Rec)).

e6(Rec) ->
    meta:splice(e5_m(meta:quote(Rec))).

e7(_Rec) ->
    meta:quote(meta:splice(e5_m(meta:quote(Rec)))).


le() ->
    meta:quote(1+2).

sle() ->
    meta:splice(le()).

%% err() ->
%%     meta:splice(5).
