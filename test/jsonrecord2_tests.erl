%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2012, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 12 Jul 2012 by Eduard Sergeev
%%%-------------------------------------------------------------------
-module(jsonrecord2_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/jsonrecord2.hrl").


-type my_integer() :: integer().

%% -type my_list(A) :: [A].


-record(untyped_rec, {id, f1, f2}).

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


to_struct(Rec) when is_integer(Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(my_integer()),
      meta:reify());
to_struct(Rec) when is_atom(Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(status()),
      meta:reify());
to_struct(#untyped_rec{} = Rec) ->
    encode_gen(
      meta:quote(Rec),
      meta:reify_type(#untyped_rec{}),
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

from_struct(integer, Struct) ->
    decode_gen(
      meta:quote(Struct),
      meta:reify_type(my_integer()),
      meta:reify());
from_struct(atom, Struct) ->
    decode_gen(
      meta:quote(Struct),
      meta:reify_type(status()),
      meta:reify());
from_struct(untyped_rec, Struct) ->
    decode_gen(
      ?q(Struct),
      meta:reify_type(#untyped_rec{}),
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


untyped_test() ->
    Rec = #untyped_rec{id = 42, f1 = <<"Bin">>, f2 = atom},
    Struct = to_struct(Rec),
    Restored = from_struct(untyped_rec, Struct),
    ?assertEqual(Rec, Restored).


rec0_test() ->
    Rec = #rec0
        {id = 42,
         an = <<"Bin">>,
         atom = atom},
    Struct = to_struct(Rec),
    Restored = from_struct(rec0, Struct),
    ?assertEqual(Rec, Restored).
