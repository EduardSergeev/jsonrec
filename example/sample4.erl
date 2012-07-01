-module(sample4).

-compile(export_all).
-compile({parse_transform, meta}).

-import(sample5,[test/1]).
-meta([test/1]).

%% -type a() :: integer().

-record(rec, {id, field}).

s0() ->
    1.

s1() ->
    meta:quote(1).

s15() ->
    s1().

s2(E) ->
   meta:quote(meta:splice(E)+1).

s3() ->
   meta:splice(s1()).

s35() ->
   s2(meta:quote(1)).

s4(A) ->
   meta:splice(s2(meta:quote(A))).

s45() ->
    meta:splice(erl_parse:abstract(f(23))).

f(N) ->
    N*2.

s5() ->
   meta:quote(meta:splice({integer,0,1})).

s6() ->
   meta:quote(1+2).

%% s7() ->
%%    meta:reify(fun s2/1).


s8() ->
    meta:splice(s15()).


%% s9() ->
%%     meta:reify_types().

s10(#rec{id = Id} = Rec) ->
    meta:splice(spl(meta:quote(Rec), meta:quote(Id))).
    

spl(A, B) ->
    meta:quote({meta:splice(A),meta:splice(B)}).
