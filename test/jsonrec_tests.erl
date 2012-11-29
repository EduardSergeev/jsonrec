%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2012, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 16 Jul 2012 by Eduard Sergeev
%%%-------------------------------------------------------------------
-module(jsonrec_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/jsonrec.hrl").

-compile(export_all).

%% -type status() :: new | old | unknown.

%% -record(rec0,
%%          {sta :: status()}).

%% -record(rec4,
%%         {id2 :: integer()}).

%% -record(rec3,
%%         {id :: integer(),
%%          f1 :: float(),
%%          rec :: #rec4{} }).

-type my_integer() :: integer().
-type my_list(A) :: [A].


-record(rec0,
        {id :: integer(),
        status = new :: status(),
        atom :: atom(),
        binary :: binary(),
        boolean :: boolean()}).

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


%% -type any_rec() :: #rec0{} | #rec1{}.

%% %% -decode({{any_rec,[]}, {any,[]}}).

%% -record(rec3,
%%         {id = 0 :: integer(),
%%          type = rec0 :: rec0 | rec1,         %% we need this descriptor
%%          rec = #rec0{id = 1} :: any_rec()}). %% to be able to decode 'rec' field


-type status() :: new | old | unknown | my_atom().

-record(rec4, 
        {id :: integer(),
         status = new :: status() }).

%% -record(rec2,
%%         {rs :: [#rec1{}]}).



encode(Rec) when is_integer(Rec) ->
    ?encode_gen(my_integer(), Rec);
encode(Rec) when is_atom(Rec) ->
    ?encode_gen(status(), Rec);
encode(#rec0{} = Rec) ->
    ?encode_gen(#rec0{}, Rec);
encode(#rec1{} = Rec) ->
    ?encode_gen(#rec1{}, Rec);
encode(#rec2{} = Rec) ->
    ?encode_gen(#rec2{}, Rec);
%% encode(#rec3{} = Rec) ->
%%     ?encode_gen(#rec3{}, Rec);
encode(#rec4{} = Rec) ->
    ?encode_gen(#rec4{}, Rec).

decode(integer, Struct) ->
    ?decode_gen(my_integer(), Struct);
decode(atom, Struct) ->
    ?decode_gen(status(), Struct);
decode(rec0, Struct) ->
    ?decode_gen(#rec0{}, Struct);
decode(rec1, Struct) ->
    ?decode_gen(#rec1{}, Struct);
decode(rec2, Struct) ->
    ?decode_gen(#rec2{}, Struct);
%% decode(rec3, Struct) ->
%%     #rec3{type = Type, rec = Str} = Rec = ?decode_gen(#rec3{}, Struct),
%%     Rec#rec3{rec = decode(Type, Str)};
decode(rec4, Struct) ->
    ?decode_gen(#rec4{}, Struct).


decode_test_() ->
    [{"Simple decode",
      ?_test(
         begin
             Inp = <<"{
                       \"Id\":42,
                       \"Atom\" : \"status\",
                       \"Binary\"   :\"Some string\",
                       \"Boolean\": true
                      }">>,
             ?assertMatch(
                {ok, #rec0{id = 42, status = new,
                           atom = status, binary = <<"Some string">>}},
                decode(rec0, Inp))
         end)},
    {"Nested record decode",
     ?_test(
        begin
            Inp =
                <<"{
                    \"Id\":1,
                    \"Rec\": 
                     {
                      \"Id\":2,
                      \"Status\"  : \"old\",
                      \"Atom\" : \"status\",
                      \"Binary\"   :\"\",
                      \"Boolean\": false
                     }
                  }">>,
            ?assertMatch(
               {ok, #rec1{id = 1,
                          rec = #rec0{id = 2, status = old, atom = status,
                                       boolean = false, binary = <<>>}}},
               decode(rec1, Inp))
        end)}].

rec0_test() ->
    Rec = #rec0
        {id = 42,
         status = old,
         atom = some_atom,
         binary = <<"Bin">>,
         boolean = false},
    decode_encode(rec0, Rec).

rec1_test() ->
    Rec = #rec1
        {id = 1,
         rec = #rec0{id = 2, status = unknown},
         recs = [#rec0{id = 3, atom = another_atom},
                 #rec0{id = 4, binary = <<"B2">>}],
         fi = <<"La">>},
    decode_encode(rec1, Rec).

rec2_test() ->
    Rec = #rec2
        {id = 24,
         ref_ids = [1,2,3,4],
         recs0 = [#rec0{id = 43, boolean = true}],
         arr = [5]},
   decode_encode(rec2, Rec).

%% rec3_test() ->
%%     Rec0 = #rec3{id = 1},
%%     Rec1 = #rec3
%%         {id = 2,
%%          type = rec1,
%%          rec = #rec1{id = 2}},
%%     decode_encode(rec3, Rec0),
%%     decode_encode(rec3, Rec1).


%%
%% Round-trip encoding/decoding
%%    
decode_encode(Tag, Item) ->
    IoList = encode(Item),
    Json = list_to_binary(IoList),
    %% ?debugFmt("~p~n", [Json]),
    {ok, Restored} = decode(Tag, Json),
    ?assertEqual(Item, Restored).
