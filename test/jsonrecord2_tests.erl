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

%%-compile(export_all).

-type my_integer() :: integer().
-type my_list(A) :: [A].

%% -type my_tuple() :: {integer(),{float(),{integer(),float()}}}.
%% -type my_tuple(A,B) :: {A,{B,{A,B}}}.

-record(untyped_rec, {id, f1, f2}).

-record(rec0,
        {id :: my_integer(),
         an :: any(),
         atom :: atom(),
         some_field = true :: boolean()}).

-record(rec1,
        {id = 0 :: integer(),
         rec = #rec0{id = 42} :: #rec0{},
         recs = [] :: my_list(#rec0{}),
         fi = <<>> :: binary()}).

-type my_rec() :: #rec1{}.
-type my_atom() :: some_atom.

-record(rec2,
        {id :: my_integer(),
         ref_ids :: my_list(my_integer()),
         recs0 :: [#rec0{}],
         arr = [] :: [my_integer()],
         rec1 = [#rec1{}]:: [my_rec()]}).


-type any_rec() :: #rec0{} | #rec1{}.

-decode({{any_rec,[]}, {any,[]}}).

-record(rec3,
        {id = 0 :: integer(),
         type = rec0 :: rec0 | rec1,         %% we need this descriptor
         rec = #rec0{id = 1} :: any_rec()}). %% to be able to decode 'rec' field


-type status() :: new | sent | loaded | my_atom().

-record(rec4, 
        {id :: integer(),
         status = new :: status() }).


to_struct(Rec) when is_integer(Rec) ->
    ?encode_gen(my_integer(), Rec);
to_struct(Rec) when is_atom(Rec) ->
    ?encode_gen(status(), Rec);
to_struct(#untyped_rec{} = Rec) ->
    ?encode_gen(#untyped_rec{}, Rec);
to_struct(#rec0{} = Rec) ->
    ?encode_gen(#rec0{}, Rec);
to_struct(#rec1{} = Rec) ->
    ?encode_gen(#rec1{}, Rec);
to_struct(#rec2{} = Rec) ->
    ?encode_gen(#rec2{}, Rec);
to_struct(#rec3{} = Rec) ->
    ?encode_gen(#rec3{}, Rec);
to_struct(#rec4{} = Rec) ->
    ?encode_gen(#rec4{}, Rec).

from_struct(integer, Struct) ->
    ?decode_gen(my_integer(), Struct);
from_struct(atom, Struct) ->
    ?decode_gen(status(), Struct);
from_struct(untyped_rec, Struct) ->
    ?decode_gen(#untyped_rec{}, Struct);
from_struct(rec0, Struct) ->
    ?decode_gen(#rec0{}, Struct);
from_struct(rec1, Struct) ->
    ?decode_gen(#rec1{}, Struct);
from_struct(rec2, Struct) ->
    ?decode_gen(#rec2{}, Struct);

from_struct(rec3, Struct) ->
    #rec3{type = Type, rec = Str} = Rec = ?decode_gen(#rec3{}, Struct),
    Rec#rec3{rec = from_struct(Type, Str)};

from_struct(rec4, Struct) ->
    ?decode_gen(#rec4{}, Struct).


untyped_test() ->
    Rec = #untyped_rec
        {id = 42,
         f1 = <<"Bin">>,
         f2 = true},
    decode_encode(untyped_rec, Rec).


rec0_test() ->
    Rec = #rec0
        {id = 42,
         an = <<"Bin">>,
         atom = atom},
    decode_encode(rec0, Rec).

rec1_test() ->
    Rec = #rec1
        {id = 1,
         rec = #rec0{id = 2, an = 42.5},
         recs = [#rec0{id = 3, an = false}, #rec0{id = 4, an = $#}],
         fi = <<"La">>},
    decode_encode(rec1, Rec).

rec2_test() ->
    Rec = #rec2
        {id = 24,
         ref_ids = [1,2,3,4],
         recs0 = [#rec0{id = 43, an = false}],
         arr = [5]},
   decode_encode(rec2, Rec).

rec3_test() ->
    Rec0 = #rec3{id = 1},
    Rec1 = #rec3
        {id = 2,
         type = rec1,
         rec = #rec1{id = 2}},
    decode_encode(rec3, Rec0),
    decode_encode(rec3, Rec1).


%%
%% 'name_conv' option test
%%
to_upper_struct(#rec0{} = Rec) ->
    ?encode_gen(#rec0{}, Rec,
                [{name_handler, fun jr_test_remote:to_upper/1}]).

from_upper_struct(Struct) ->
    ?decode_gen(#rec0{}, Struct,
                [{name_handler,
                  fun(Field) ->
                          Str = atom_to_list(Field),
                          UStr = string:to_upper(Str),
                          list_to_binary(UStr)
                  end}]).
    
name_conv_test() ->
    Rec = #rec0
        {id = 42,
         an = 4.2,
         atom = some_atom,
         some_field = false},
    Struct = to_upper_struct(Rec),
    ?assertEqual({struct,
                  [{<<"ID">>,42},
                   {<<"AN">>,4.2},
                   {<<"ATOM">>,<<"some_atom">>},
                   {<<"SOME_FIELD">>,false}]},
                Struct),
    Restored = from_upper_struct(Struct),
    ?assertEqual(Rec, Restored),
    JS = mochijson2:encode(Struct),
    Struct1 = mochijson2:decode(JS),
    Restored1 = from_upper_struct(Struct1),
    ?assertEqual(Rec, Restored1).

%%
%% Round-trip encoding/decoding
%%    
decode_encode(Tag, Item) ->
    Struct = to_struct(Item),
    Restored = from_struct(Tag, Struct),
    ?assertEqual(Item, Restored),
    JS = mochijson2:encode(Struct),
    Struct1 = mochijson2:decode(JS),
    Restored1 = from_struct(Tag, Struct1),
    ?assertEqual(Item, Restored1).
    
