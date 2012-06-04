-module(rec).

-include("../src/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen_ms/1, decode_gen_ms/1]).
-meta([encode_gen_ms/1, decode_gen_ms/1, reify_record/1]).

-record(rec1, {id, fi}).
-record(rec2, {id, rec1, int, f1, f2, f3}).

encode(#rec1{} = Rec) ->
    meta:splice(
      jsonrecord2:encode_gen_ms(
        Rec,
        meta:reify(#rec1{})));
encode(#rec2{} = Rec) ->
    meta:splice(
      jsonrecord2:encode_gen_ms(
        Rec,
        meta:reify(#rec2{}))).

decode(rec1, Binary) ->
   meta:splice(
     jsonrecord2:decode_gen_ms(
       Binary,
       meta:reify(#rec1{})));
decode(rec2, Binary) ->
   meta:splice(
     jsonrecord2:decode_gen_ms(
       Binary,
       meta:reify(#rec2{}))).
