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

%% -compile(export_all).

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
         rec1 = [#rec1{}] :: [my_rec()]}).


-type any_rec() :: #rec0{} | #rec1{}.
-record(rec3,
        {id = 0 :: integer(),
         type = rec0 :: rec0 | rec1,         %% we need this descriptor
         rec = #rec0{id = 1} :: any_rec()}). %% to be able to decode 'rec' field


-type status() :: new | old | unknown | my_atom().

-record(rec4, 
        {id :: integer(),
         status = new :: status() }).

-record(rec5, 
        {id :: integer(),
         any :: any()}).

-record(rec6, 
        {id :: integer(),
         string :: string()}).

-record(rec_rec,
        {id :: integer(),
         rec :: #rec_rec{}}).

-jsonrec([{encoder, {{record,rec_rec}, encoder}},
          {parser, {{record,rec_rec}, rec_rec_parser}}]).


-record(rec7, 
        {f1 :: atom1,
         f2 = atom2 :: atom2,
         f3 :: undefined,
         f4 = undefined :: undefined}).


-type rec8_tuple() :: {string(), integer()}.
-record(rec8,
        {f1 :: rec8_tuple(),
         f2 :: my_integer()}).

-record(rec9,
        {list_or_rec :: [#rec6{}] | #rec6{},
         rec_or_list :: #rec6{} | [#rec6{}]}).


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
    ?encode_gen(#rec4{}, Rec);
encode(#rec5{} = Rec) ->
    ?encode_gen(#rec5{}, Rec);
encode(#rec6{} = Rec) ->
    ?encode_gen(#rec6{}, Rec);
encode(#rec_rec{} = Rec) ->
    ?encode_gen(#rec_rec{}, Rec);
encode(#rec7{} = Rec) ->
    ?encode_gen(#rec7{}, Rec);
encode(#rec8{} = Rec) ->
    ?encode_gen(
       #rec8{}, Rec,
       [{encoder,
         {{rec8_tuple, []}, rec8_tuple_encoder}},
        {surrogate_type,
         {{my_integer, []}, {integer, []}}}]);
encode(#rec9{} = Rec) ->
    ?encode_gen(#rec9{}, Rec).


decode(integer, Bin) ->
    ?decode_gen(my_integer(), Bin);
decode(atom, Bin) ->
    ?decode_gen(status(), Bin);
decode(rec0, Bin) ->
    ?decode_gen(#rec0{}, Bin);
decode(rec1, Bin) ->
    ?decode_gen(#rec1{}, Bin);
decode(rec2, Bin) ->
    ?decode_gen(#rec2{}, Bin);
decode(rec3, Bin) ->
    {ok, #rec3{type = Type, rec = RecBin} = Rec} =
        ?decode_gen(
           #rec3{}, Bin,
           [{surrogate_type, {{any_rec,[]}, {any,[]}}}]),
    case decode(Type, RecBin) of
        {ok, RecField} ->
            {ok, Rec#rec3{rec = RecField}};
        Error ->
            Error
    end;
decode(rec4, Bin) ->
    ?decode_gen(#rec4{}, Bin);
decode(rec5, Bin) ->
    ?decode_gen(#rec5{}, Bin);
decode(rec6, Bin) ->
    ?decode_gen(#rec6{}, Bin);
decode(rec_rec, Bin) ->
    ?decode_gen(#rec_rec{}, Bin);
decode(rec7, Bin) ->
    ?decode_gen(#rec7{}, Bin);
decode(rec8, Bin) ->
    ?decode_gen(
       #rec8{}, Bin,
       [{parser,
         {{rec8_tuple, []}, rec8_tuple_parser}},
        {surrogate_type,
         {{my_integer, []}, {integer, []}}}]);
decode(rec9, Bin) ->
    ?decode_gen(#rec9{}, Bin).


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
         {error, {expected, <<"{">>, at, <<"\"id\":42}">>}},
         decode(rec0, <<"\"id\":42}">>))},
     {"Invalid format: missing }",
      ?_assertMatch(
         {error, _},
         decode(rec0, <<"{\"id\":42">>))},
     {"Invalid format: missing deilimiter :",
      ?_assertMatch(
         {error, _},
         decode(rec0, <<"{\"id\"42}">>))},
     {"Invalid content",
      ?_assertMatch(
         {error, _},
         decode(rec0, <<"{\"id\":\"wrong\"}">>))}].


rec0_test() ->
    Rec = #rec0
        {id = 42,
         status = old,
         atom = some_atom,
         binary = <<"Bin">>,
         boolean = false},
    encode_decode(rec0, Rec).

rec1_test() ->
    Rec = #rec1
        {id = 1,
         rec = #rec0{id = 2, status = unknown},
         recs = [#rec0{id = 3, atom = another_atom},
                 #rec0{id = 4, binary = <<"B2">>}],
         fi = <<"La">>},
    encode_decode(rec1, Rec).

rec2_test() ->
    Rec = #rec2
        {id = 24,
         ref_ids = [1,2,3,4],
         recs0 = [#rec0{id = 43, boolean = true}],
         arr = [5]},
   encode_decode(rec2, Rec).

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
    encode_decode(rec3, Rec0),
    encode_decode(rec3, Rec1).

whitespace_test() ->
    Rec = #rec3{id = 1},
    Json = list_to_binary(encode(Rec)),
    Json1 = <<"\n", Json/binary>>,
    ?assertMatch({ok, Rec}, decode(rec3, Json1)).

extra_fields_test() ->
    Rec = #rec5{id = 1},
    Json = <<"{\"id\": 1, \"new_field\": 42}">>,
    ?assertMatch({ok, Rec},
                 decode(rec5, Json)).
    
string_escape_test() ->
    Bin = <<"\n1\t2\n\r3\b4\"5\\">>,
    Rec = #rec0{another_field = Bin},
    Json = list_to_binary(encode(Rec)),
    ?assertMatch(<<"{\"status\":\"new\","
                   "\"another_field\":\"\\n1\\t2\\n\\r3\\b4\\\"5\\\\\"}">>,
                Json),
    ?assertMatch({ok, #rec0{another_field = Bin}},
                 decode(rec0, Json)).

string_test() ->
    Rec = #rec6{id = 1, string = "Some string\n\r"},
    encode_decode(rec6, Rec),
    Json = list_to_binary(encode(Rec)),
    ?assertMatch(<<"{\"id\":1,\"string\":\"Some string\\n\\r\"}">>,
                 Json).
    

simple_list_test() ->
    E = fun(Rec) ->
                ?encode_gen([integer()], Rec)
        end,
    D = fun(Bin) ->
                ?decode_gen([integer()], Bin)
        end,
    Rs = [1,2,3],
    IoList = E(Rs),
    Json = list_to_binary(IoList),
    {ok, Rs1} = D(Json),
    ?assertEqual(Rs, Rs1).

record_list_test() ->
    E = fun(Rec) ->
                ?encode_gen([#rec6{}], Rec)
        end,
    D = fun(Bin) ->
                ?decode_gen([#rec6{}], Bin)
        end,
    Rs = [#rec6{id = 1, string = "test1"},
          #rec6{id = 2, string = "test2"}],
    IoList = E(Rs),
    Json = list_to_binary(IoList),
    {ok, Rs1} = D(Json),
    ?assertEqual(Rs, Rs1).

union_list_test() ->
    R6 = #rec6{id = 1, string = "Some string\n\r"},
    Rec1 = #rec9{rec_or_list = R6, list_or_rec = R6},
    encode_decode(rec9, Rec1),
    Rec2 = #rec9{rec_or_list = [R6,R6], list_or_rec = [R6,R6,R6]},
    encode_decode(rec9, Rec2).
    

%%
%% Recursive definitions tests
%%
encoder(#rec_rec{} = Rec) ->
    ?encode_gen_encoder(#rec_rec{}, Rec).

rec_rec_parser(Bin) ->
    ?decode_gen_parser(#rec_rec{}, Bin).

self_reference_test() ->
    Rec = #rec_rec
        {id = 1,
         rec = #rec_rec
         {id = 2,
          rec = #rec_rec
          {id = 3}}},
    encode_decode(rec_rec, Rec).

undefined_atom_test_() ->
    [{"Default values",
      ?_test(encode_decode(rec7, #rec7{}))},
     {"All values are set",
      ?_test(encode_decode(rec7, #rec7{f1 = atom1, f2 = atom2}))}].
    


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


to_camel_json(#rec0{} = Rec) ->
    ?encode_gen(
       #rec0{}, Rec,
       [{name_handler, fun jsonrec:atom_to_camel/1}]).

from_camel_json(Bin) ->
    ?decode_gen(
       #rec0{}, Bin,
       [{name_handler, fun jsonrec:atom_to_camel/1}]).

to_camel_conv_test() ->
    Rec = #rec0
        {id = 42,
         atom = some_atom,
         boolean = false,
         another_field = <<"B">>},
    Bin = list_to_binary(to_camel_json(Rec)),
    ?assertEqual(<<"{\"id\":42,"
                   "\"status\":\"new\","
                   "\"atom\":\"some_atom\","
                   "\"boolean\":false,"
                   "\"anotherField\":\"B\"}">>,
                 Bin),
    Restored = from_camel_json(Bin),
    ?assertEqual({ok, Rec}, Restored).


nested_error_test() ->
    Json = <<"{\"id\": 42,"
             " \"recs\": ["
             "  {\"id\": 1},"
             "  {\"id\": 2,"
             "   \"binary\": \"\\g\"}]}">>,
    ?assertMatch({error, _},
                 decode(rec1, Json)).

anyjson_error_test() ->
    Json = <<"{\"id\": 42,"
             " \"any\": ["
             "  {\"id\": 1},"
             "  {\"id\": 2,"
             "   \"binary\": \"\\g\"}]}">>,
    ?assertMatch({error, _},
                 decode(rec5, Json)).


rec0_parser(Inp) ->
    ?decode_gen_parser(#rec0{}, Inp).

rec5_decode_any_as_rec0(Inp) ->
    case ?decode_gen_parser(#rec5{}, Inp) of
        {ok, {#rec5{any = Rec0Bin} = Rec5, _}} ->
            case rec0_parser(Rec0Bin) of
                {ok, {Rec0, _}} ->
                    {ok, Rec5#rec5{any = Rec0}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.
    
custom_parser_error_test() ->
    Json = <<"{\"id\": 1,"
             " \"any\": {\"id\": true}}">>,
    ?assertMatch({error, _},
                 rec5_decode_any_as_rec0(Json)).
    
%%
%% Customised encoding/decoding tests
%%
rec8_tuple_encoder({String, Integer}) ->
    [$\", list_to_binary(String ++ "/" ++ integer_to_list(Integer)), $\"].

rec8_tuple_parser(Bin) ->
    case json_parsers:string_p(Bin) of
        {ok, {BStr, Bin1}} ->
            List = binary_to_list(BStr),
            case string:tokens(List, "/") of
                [String, SInteger] ->
                    {ok, {{String, list_to_integer(SInteger)}, Bin1}};
                Inv ->
                    {error, {invalid_rec8_tuple, Inv}}
            end;
        Error ->
            Error
    end.
    
customised_gen_test() ->
    Rec = #rec8
        {f1 = {"test", 42},
         f2 = 43},
    Bin = list_to_binary(encode(Rec)),
    ?assertMatch(<<"{\"f1\":\"test/42\","
                   "\"f2\":43}">>, Bin),
    encode_decode(rec8, Rec).




%%
%% Round-trip encoding/decoding
%%    
encode_decode(Tag, Item) ->
    IoList = encode(Item),
    Json = list_to_binary(IoList),
    %% ?debugFmt("~p~n", [Json]),
    {ok, Restored} = decode(Tag, Json),
    ?assertEqual(Item, Restored).
