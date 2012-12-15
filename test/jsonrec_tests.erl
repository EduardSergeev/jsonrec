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

-type my_integer() :: integer().
-type my_list(A) :: [A].


-record(rec0,
        {id :: integer(),
        status = new :: status(),
        atom :: atom(),
        binary :: binary(),
        boolean :: boolean(),
        another_field = <<>> :: binary()}).

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
encode(#rec3{} = Rec) ->
    ?encode_gen(#rec3{}, Rec);
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
decode(rec3, Struct) ->
    {ok, #rec3{type = Type, rec = Bin} = Rec} = ?decode_gen(#rec3{}, Struct),
    {ok, RecField} = decode(Type, Bin),
    {ok, Rec#rec3{rec = RecField}};
decode(rec4, Struct) ->
    ?decode_gen(#rec4{}, Struct).


decode_test_() ->
    [{"Simple decode",
      ?_test(
         begin
             Inp = <<"{
                       \"id\":42,
                       \"atom\" : \"status\",
                       \"binary\"   :\"Some string\",
                       \"boolean\": true
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
                    \"id\":1,
                    \"rec\": 
                     {
                      \"id\":2,
                      \"status\"  : \"old\",
                      \"atom\" : \"status\",
                      \"binary\"   :\"\",
                      \"boolean\": false
                     }
                  }">>,
            ?assertMatch(
               {ok, #rec1{id = 1,
                          rec = #rec0{id = 2, status = old, atom = status,
                                       boolean = false, binary = <<>>}}},
               decode(rec1, Inp))
        end)}].

decode_error_test_() ->
    [{"Invalid format: missing {",
      ?_assertMatch(
         {error, {{expected, <<"{">>}, <<"\"id\":42}">>}},
         decode(rec0, <<"\"id\":42}">>))},
     {"Invalid format: missing }",
      ?_assertMatch(
         {error, {{expected, <<"}">>}, <<>>}},
         decode(rec0, <<"{\"id\":42">>))}].
     %% {"Invalid format: missing deilimiter :",
     %%  ?_assertMatch(
     %%     {error, {{expected, <<":">>}, 5}},
     %%     decode(rec0, <<"{\"Id\"42}">>))},
     %% {"Invalid content",
     %%  ?_assertMatch(
     %%     {error, {_, 6}},
     %%     decode(rec0, <<"{\"Id\":\"wrong\"}">>))}].


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

rec3_decode_test() ->
    Rec = #rec3
        {id = 1,
         type = rec1,
         rec = #rec1{id = 2}},
    Json = <<"{\"id\":1,\"type\":\"rec1\",\"rec\":{\"id\":2}}">>,
    ?assertMatch({ok, Rec}, decode(rec3, Json)).

rec3_test() ->
    Rec0 = #rec3{id = 1},
    Rec1 = #rec3
        {id = 2,
         type = rec1,
         rec = #rec1{id = 11}},
    decode_encode(rec3, Rec0),
    decode_encode(rec3, Rec1).

whitespace_test() ->
    Rec = #rec3{id = 1},
    Json = list_to_binary(encode(Rec)),
    Json1 = <<"\n", Json/binary>>,
    ?assertMatch({ok, Rec}, decode(rec3, Json1)).
    

%%
%% 'name_conv' option test
%%
to_upper_json(#rec0{} = Rec) ->
    ?encode_gen(#rec0{}, Rec,
                [{name_handler, fun jr_test_remote:to_upper_string/1}]).

from_upper_json(Struct) ->
    ?decode_gen(#rec0{}, Struct,
                [{name_handler,
                  fun(Field) ->
                          Str = atom_to_list(Field),
                          string:to_upper(Str)
                  end}]).
    
to_upper_conv_test() ->
    Rec = #rec0
        {id = 42,
         atom = some_atom,
         boolean = false},
    Bin = list_to_binary(to_upper_json(Rec)),
    ?assertEqual(<<"{\"ID\":42,"
                   "\"STATUS\":\"new\","
                   "\"ATOM\":\"some_atom\","
                   "\"BOOLEAN\":false,"
                   "\"ANOTHER_FIELD\":\"\"}">>,
                 Bin),
    Restored = from_upper_json(Bin),
    ?assertEqual({ok, Rec}, Restored).


to_pascal_json(#rec0{} = Rec) ->
    ?encode_gen(#rec0{}, Rec,
                [{name_handler, fun jsonrec:atom_to_pascal/1}]).

from_pascal_json(Bin) ->
    ?decode_gen(#rec0{}, Bin,
                [{name_handler, fun jsonrec:atom_to_pascal/1}]).

to_pascal_conv_test() ->
    Rec = #rec0
        {id = 42,
         atom = some_atom,
         boolean = false,
         another_field = <<"B">>},
    Bin = list_to_binary(to_pascal_json(Rec)),
    ?assertEqual(<<"{\"Id\":42,"
                   "\"Status\":\"new\","
                   "\"Atom\":\"some_atom\","
                   "\"Boolean\":false,"
                   "\"AnotherField\":\"B\"}">>,
                 Bin),
    Restored = from_pascal_json(Bin),
    ?assertEqual({ok, Rec}, Restored).


%%
%% Round-trip encoding/decoding
%%    
decode_encode(Tag, Item) ->
    IoList = encode(Item),
    Json = list_to_binary(IoList),
    %% ?debugFmt("~p~n", [Json]),
    {ok, Restored} = decode(Tag, Json),
    ?assertEqual(Item, Restored).
