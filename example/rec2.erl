-module(rec2).

-include_lib("meta/include/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen/3, decode_gen/3]).
-meta([encode_gen/3]).


-type my_integer() :: integer().

-record(rec0,
        {id :: my_integer(),
         an = 42 :: any(),
         some_field = true :: boolean()}).

-record(rec1,
        {id = 0 :: integer(),
         rec :: #rec0{},
         recs :: [#rec0{}],
         fi = <<>> :: binary()}).

-type my_rec() :: #rec1{}.
%%-type my_atom() :: some_atom.

-record(rec2,
        {id :: my_integer(),
         rec0 :: [#rec0{}],
         arr = [] :: [my_integer()],
         rec1 = [#rec1{}]:: [my_rec()]}).

-type any_rec() :: #rec0{} | #rec1{}.
-type status() :: new | sent | loaded.

-record(rec3,
        {id = 0 :: integer(),
         rec :: any_rec()}).

-record(rec4, 
        {id :: integer(),
         status :: status() }).



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
      meta:reify());
to_struct(#rec3{} = Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(#rec3{}),
      meta:reify());
to_struct(#rec4{} = Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(#rec4{}),
      meta:reify()).


%% from_struct(rec2, Struct) ->
%%     ?s(decode_gen(
%%          ?q(Struct),
%%          meta:reify_type(#rec2{}),
%%          meta:reify()));
%% from_struct(rec0, Struct) ->
%%     ?s(decode_gen(
%%          ?q(Struct),
%%          meta:reify_type(#rec0{}),
%%          meta:reify())).

