-module(rec2).

-include_lib("meta/include/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen/3, decode_gen/3]).
-meta([encode_gen/3]).


-type my_integer() :: integer().

-record(rec0,
        {id :: my_integer(),
         some_field = true :: boolean()}).

-record(rec1,
        {id :: integer(),
         fi = <<>> :: binary()}).

-type my_rec() :: #rec1{}.

-record(rec2,
        {id :: integer(),
         rec0 :: #rec0{},
         arr = [] :: [my_integer()],
         rec1 = [#rec1{}]:: [my_rec()]}).


to_struct(#rec0{} = Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(#rec0{}),
      meta:reify());
to_struct(#rec1{} = Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(#rec1{}),
      meta:reify());
to_struct(#rec2{} = Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(#rec2{}),
      meta:reify()).

from_struct(Struct) ->
    ?s(decode_gen(
         ?q(Struct),
         meta:reify_type(#rec2{}),
         meta:reify())).
