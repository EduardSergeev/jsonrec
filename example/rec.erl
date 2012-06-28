-module(rec).

-include("../src/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen_ms/4, decode_gen_ms/4]).

-meta([encode_gen_ms/4, decode_gen_ms/4]).

-record(rec1,
        {id :: integer(),
         fi = <<>> :: binary()}).
-record(rec3,
        {id = 42 :: integer(), field1}).

-record(rec4,
         {id :: integer(), field2}).

-type rec1_4() :: #rec1{} | #rec4{}.

-type id() :: integer().

-type elem(Id) :: {Id,float(),atom()}.

-type int_elem() :: elem(integer()).


-record(rec2,
        {id,
         rec1 = #rec1{id = 0} :: #rec1{},
         recs3 = [] :: [#rec3{}],
         f1 = false,
         f2,
         f3}).

to_struct(#rec1{} = Rec) ->
    encode_gen_ms(
      Rec,
      meta:reify(#rec1{}),
      meta:reify_type(#rec1{}),
      to_struct);
to_struct(#rec2{} = Rec) ->
    encode_gen_ms(
      Rec,
      meta:reify(#rec2{}),
      meta:reify_type(#rec2{}),
      to_struct);
to_struct(#rec3{} = Rec) ->
    encode_gen_ms(
      Rec,
      meta:reify(#rec3{}),
      meta:reify_type(#rec3{}),
      to_struct).

to_struct2(#rec1{} = Rec) ->
    jsonrecord2:encode_gen(
      meta:line(),
      rec1,
      meta:reify_types(),
      Rec).


encode(Rec) ->
    mochijson2:encode(to_struct(Rec)).


from_struct(rec1, Struct) ->
    decode_gen_ms(
      Struct,
      meta:reify(#rec1{}),
      meta:reify_type(#rec1{}),
      from_struct);
from_struct(rec2, Struct) ->
    decode_gen_ms(
      Struct,
      meta:reify(#rec2{}),
      meta:reify_type(#rec2{}),
      from_struct);
from_struct(rec3, Struct) ->
    decode_gen_ms(
      Struct,
      meta:reify(#rec3{}),
      meta:reify_type(#rec3{}),
      from_struct).


decode(Type, Binary) ->
    from_struct(Type,
                mochijson2:decode(Binary)).


%% types() ->
%%     meta:reify_types().
