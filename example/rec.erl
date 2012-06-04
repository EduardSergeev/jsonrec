-module(rec).

-include("../src/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen_ms/1, decode_gen_ms/1]).
-meta([encode_gen_ms/1, decode_gen_ms/1, reify_record/1]).

-record(rec1,
        {id :: integer(),
         fi :: binary()}).
-record(rec2,
        {id,
         rec1 :: #rec1{},
         int,
         f1,
         f2,
         f3}).

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

t2(#rec2{} = Rec) ->
    jsonrecord2:encode_gen_ms2(
      Rec,
      meta:reify(#rec2{}),
      meta:reify_type(#rec2{}),
      [{rec1,fun encode/1}]).

t3(#rec1{} = Rec) ->
    jsonrecord2:encode_gen_ms2(
      Rec,
      meta:reify(#rec1{}),
      meta:reify_type(#rec1{}),
      []).

to_struct(#rec1{} = Rec) ->
    meta:splice(
      jsonrecord2:encode_gen_ms2(
        Rec,
        meta:reify(#rec1{}),
        meta:reify_type(#rec1{}),
        []));
to_struct(#rec2{} = Rec) ->
    meta:splice(
      jsonrecord2:encode_gen_ms2(
        Rec,
        meta:reify(#rec2{}),
        meta:reify_type(#rec2{}),
        [{rec1, to_struct}])).

encode2(Rec) ->
    mochijson2:encode(to_struct(Rec)).


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

t1() ->
    meta:reify_type(#rec2{}).
