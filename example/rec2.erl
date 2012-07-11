-module(rec2).

-include_lib("meta/include/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen/3, decode_gen/3]).
-meta([encode_gen/3, decode_gen/3]).


-type my_integer() :: integer().

-type my_list(A) :: [A].

-spec f1(any()) -> any().
                

-record(rec_, {id}).

-record(rec0,
        {id :: my_integer(),
         an :: any(),
         atom :: atom(),
         some_field = true :: boolean()}).

-record(rec1,
        {id = 0 :: integer(),
         rec = #rec0{id = 42} :: #rec0{},
         recs = [] :: [#rec0{}],
%%         recs2 = [] :: my_list(#rec0{}),
         fi = <<>> :: binary()}).

-type my_rec() :: #rec1{}.
-type my_atom() :: some_atom.

-record(rec2,
        {id :: my_integer(),
         rec0 :: [#rec0{}],
         arr = [] :: [my_integer()],
         rec1 = [#rec1{}]:: [my_rec()]}).

-type any_rec() :: #rec0{} | #rec1{}.
-type status() :: new | sent | loaded | my_atom().

-record(rec3,
        {id = 0 :: integer(),
         rec = #rec0{id = 1} :: any_rec()}).

-record(rec4, 
        {id :: integer(),
         status = new :: status() }).

t1() ->
    meta:reify_type(my_integer()).

t2() ->
    meta:reify_type(#rec_{}).

t3() ->
    meta:reify_type(#rec0{}).


f1(A) ->
    A.

to_struct(#rec_{} = Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(#rec_{}),
      meta:reify());
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


from_struct(rec_, Struct) ->
    decode_gen(
      ?q(Struct),
      meta:reify_type(#rec_{}),
      meta:reify());
from_struct(rec0, Struct) ->
    decode_gen(
      ?q(Struct),
      meta:reify_type(#rec0{}),
      meta:reify());
from_struct(rec1, Struct) ->
    decode_gen(
      ?q(Struct),
      meta:reify_type(#rec1{}),
      meta:reify());
from_struct(rec2, Struct) ->
    decode_gen(
      ?q(Struct),
      meta:reify_type(#rec2{}),
      meta:reify());
%% from_struct(rec3, Struct) ->
%%     decode_gen(
%%       ?q(Struct),
%%       meta:reify_type(#rec3{}),
%%       meta:reify());
from_struct(rec4, Struct) ->
    decode_gen(
      ?q(Struct),
      meta:reify_type(#rec4{}),
      meta:reify()).


