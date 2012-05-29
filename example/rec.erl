-module(rec).

-include("../src/meta.hrl").

-compile(export_all).

-record(rec1, {id, fi}).
-record(rec2, {id, rec1, int, f1, f2, f3}).


t() ->
    meta:reify(#rec1{}).

gen1() ->
    jsonrecord2:encode_gen(meta:reify(#rec1{})).

gen2() ->
    jsonrecord2:encode_gen(meta:reify(#rec2{})).

gen3() ->
    jsonrecord2:encode_gen_ms(meta:reify(#rec2{})).

gen4() ->
    jsonrecord2:decode_gen_ms(meta:reify(#rec2{})).

-splice(gen3).
-splice(gen4).