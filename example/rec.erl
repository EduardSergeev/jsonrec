-module(rec).

-include("../src/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen_ms/4, decode_gen_ms/4]).

-meta([encode_gen_ms/4, decode_gen_ms/4]).

-record(rec1,
        {id :: integer(),
         fi = <<>> :: binary()}).
-record(rec3,
        {id :: integer(), field1}).

%% -record(rec4,
%%          {id :: integer(), field2}).

%% -type rec1_4() :: #rec1{} | #rec4{}.

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

