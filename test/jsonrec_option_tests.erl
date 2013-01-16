%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2013, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2013 by Eduard Sergeev
%%%-------------------------------------------------------------------
-module(jsonrec_option_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("meta/include/meta.hrl").

-include("../src/jsonrec.hrl").
-include("../src/json_parsers.hrl").

%% -meta_opts({dump_funs, [attribute_test_/0]}).

-compile({nowarn_unused_function, [as_is/1, to_upper/1]}).

-define(encode_decode(Encode, Decode, Rec, Expected, Opts),
        begin
            BinEncoded = list_to_binary(Encode(Rec)),
            case lists:member(debugVal, Opts) of
                true->
                    ?debugFmt("~s~n", [BinEncoded]);
                false ->
                    ok
            end,
            ?assertEqual(Expected, BinEncoded),
            ?assertMatch(
               {ok, Rec},
               Decode(BinEncoded))
        end).

-define(encode_decode(Encode, Decode, Rec, Expected),
        ?encode_decode(Encode, Decode, Rec, Expected, [])).

-define(_encode_decode(Encode, Decode, Rec, Expected, Opts),
        ?_test(?encode_decode(Encode, Decode, Rec, Expected, Opts))).

-define(_encode_decode(Encode, Decode, Rec, Expected),
        ?_test(?encode_decode(Encode, Decode, Rec, Expected))).


%%
%% Records form `Options :: [_]` arg tests
%%
-record(inner,
        {first_field :: integer(),
         second_field :: integer()}).

-record(outer,
        {field :: integer(),
         nested_record :: #inner{}}).


%%
%% Records for `-jsonrec` attribute tests
%%
-record(rec2,
        {first_field :: integer(),
         second_field :: integer(),
         third_field :: integer()}).

-record(rec3,
        {first_field :: integer(),
         second_field :: integer(),
         third_field :: integer()}).

%% This affects all records in this module
-jsonrec({name_handler, to_upper}).


%% This affects all fields of #rec3{}
-jsonrec({name_handler,
          {{record,rec3},
           {jsonrec,atom_to_pascal}}}).

%% This affects `second_field` naming of #rec3{} only
-jsonrec({name_handler,
          {{{record,rec3}, [second_field]},
           {jsonrec, atom_to_camel}}}).


-record(rec4,
        {first_field = 1 :: integer(),
         second_field = 2 :: integer(),
         third_field = 3 :: integer()}).

%% This affects naming of the last two fields of #rec4{}
-jsonrec({name_handler,
          {{{record,rec4}, [second_field, third_field]},
           {jsonrec,atom_to_pascal}}}).


-type string_integer() :: integer().
-record(rec6,
        {first_field :: string_integer(),
         second_field :: integer()}).

-jsonrec([%% Custom encoder/parser for type
          {encoder, {{string_integer, []}, encode_as_string}},
          {parser, {{string_integer, []}, integer_from_string}},
          %% Custom encoder for a field
          {encoder,
           {{{record,rec6}, [second_field]},
            encode_as_string}},
          %% Custom parser for a field
          {parser,
           {{{record,rec6}, [second_field]},
            integer_from_string}}]).

%%
%% name_handler's
%%
to_upper(Atom) ->
    string:to_upper(atom_to_list(Atom)).

as_is(Atom) ->
    atom_to_list(Atom).

%%
%% Encoders
%%
encode_as_string(Int) when is_integer(Int) ->
    [$", list_to_binary(integer_to_list(Int)), $"].

%%
%% Parsers
%%
integer_from_string(Bin) ->
    ?s(to_parser(
         right(match(?q($")),
               left(integer(), match(?q($")))),
         ?r(Bin))).

%%
%% Tests
%%

attribute_test_() ->
    [{"Global settings",
      ?_encode_decode(
        fun(Rec) -> ?encode_gen(#rec2{}, Rec) end,
        fun(Bin) -> ?decode_gen(#rec2{}, Bin) end,
         #rec2{first_field = 1, second_field = 2, third_field = 3},
         <<"{\"FIRST_FIELD\":1,\"SECOND_FIELD\":2,\"THIRD_FIELD\":3}">>)},
     {"Record-wide + specific field",
      ?_encode_decode(
         fun(Rec) -> ?encode_gen(#rec3{}, Rec) end,
         fun(Bin) -> ?decode_gen(#rec3{}, Bin) end,
         #rec3{first_field = 1, second_field = 2, third_field = 3},
         <<"{\"FirstField\":1,\"secondField\":2,\"ThirdField\":3}">>)},
     {"Global + specific field",
      ?_encode_decode(
         fun(Rec) -> ?encode_gen(#rec4{}, Rec) end,
         fun(Bin) -> ?decode_gen(#rec4{}, Bin) end,
         #rec4{first_field = 1, second_field = 2, third_field = 3},
         <<"{\"FIRST_FIELD\":1,\"SecondField\":2,\"ThirdField\":3}">>)},
     {"Custom encoder/parser",
      ?_encode_decode(
         fun(Rec) -> ?encode_gen(#rec6{}, Rec) end,
         fun(Bin) -> ?decode_gen(#rec6{}, Bin) end,
         #rec6{first_field = 1, second_field = 2},
         <<"{\"FIRST_FIELD\":\"1\",\"SECOND_FIELD\":\"2\"}">>)}
     ].

options_test_() ->
    [{"Record-wide + specific field",
      ?_encode_decode(
         fun(Rec) ->
                 ?encode_gen(#rec2{}, Rec,
                             [{name_handler,
                               [{jsonrec,atom_to_pascal},
                                {{{record,rec2}, [second_field]},
                                 {jsonrec,atom_to_camel}}]}])
         end,
         fun(Bin) ->
                 ?decode_gen(#rec2{}, Bin,
                             [{name_handler,
                               [{jsonrec,atom_to_pascal},
                                {{{record,rec2}, [second_field]},
                                 {jsonrec,atom_to_camel}}]}])
         end,
         #rec2{first_field = 1, second_field = 2, third_field = 3},
         <<"{\"FirstField\":1,\"secondField\":2,\"ThirdField\":3}">>)},
     {"Global + specific field",
      ?_encode_decode(
         fun(Rec) ->
                 ?encode_gen(#rec2{}, Rec,
                             [{name_handler,
                               {{{record,rec2}, [second_field, third_field]},
                                {jsonrec,atom_to_pascal}}}])
         end,
         fun(Bin) ->
                 ?decode_gen(#rec2{}, Bin,
                             [{name_handler,
                               {{{record,rec2}, [second_field, third_field]},
                                {jsonrec,atom_to_pascal}}}])
         end,
         #rec2{first_field = 1, second_field = 2, third_field = 3},
         <<"{\"FIRST_FIELD\":1,\"SecondField\":2,\"ThirdField\":3}">>)}].

equivalent_formats_test_() ->
    Encoders =
        [{fun(Rec) ->
                  ?encode_gen(#rec3{}, Rec)
          end,
          #rec3{first_field = 1, second_field = 2, third_field = 3}},
         {fun(Rec) ->
                  ?encode_gen(
                     #rec2{}, Rec,
                     [{name_handler,
                       [fun jsonrec:atom_to_pascal/1,
                        {{{record,rec2}, [second_field]},
                         fun jsonrec:atom_to_camel/1}]}])
          end,
          #rec2{first_field = 1, second_field = 2, third_field = 3}},
         {fun(Rec) ->
                  ?encode_gen(
                     #rec2{}, Rec,
                     [{name_handler,
                       {jsonrec,atom_to_pascal}},
                      {name_handler,
                       {{{record,rec2}, [second_field]},
                        fun jsonrec:atom_to_camel/1}}])
          end,
          #rec2{first_field = 1, second_field = 2, third_field = 3}},
         {fun(Rec) ->
                  ?encode_gen(
                     #rec4{}, Rec,
                     [{name_handler,
                       [{{{record,rec4}, [first_field]},
                         fun jsonrec:atom_to_pascal/1},
                        {{{record,rec4}, [second_field]},
                         {jsonrec,atom_to_camel}}]}])
          end,
          #rec4{first_field = 1, second_field = 2, third_field = 3}}],
    Decoders =
        [{fun(Bin) ->
                  ?decode_gen(#rec3{}, Bin)
          end,
          #rec3{first_field = 1, second_field = 2, third_field = 3}},
         {fun(Rec) ->
                  ?decode_gen(
                     #rec2{}, Rec,
                     [{name_handler,
                       [{{record,rec2},
                         fun jsonrec:atom_to_pascal/1},
                        {{{record,rec2}, [second_field]},
                         fun jsonrec:atom_to_camel/1}]}])
          end,
          #rec2{first_field = 1, second_field = 2, third_field = 3}}],
    [ ?_test(
         begin
             Bin = list_to_binary(E(Rec)),
             ?assertEqual(
                <<"{\"FirstField\":1,\"secondField\":2,\"ThirdField\":3}">>,
                Bin),
             ?assertEqual(
               {ok, Expected},
               D(Bin))
         end)
      || {E, Rec} <- Encoders,
         {D, Expected} <- Decoders ].

nested_rec_test_() ->
    Input = #outer{field = 1,
                   nested_record =
                       #inner{first_field = 2, second_field = 3}},
    [
     {"Global settings",
      ?_encode_decode(
         fun(Rec) -> ?encode_gen(#outer{}, Rec) end,
         fun(Bin) -> ?decode_gen(#outer{}, Bin) end,
         Input,
         <<"{\"FIELD\":1,"
           "\"NESTED_RECORD\":{"
           "\"FIRST_FIELD\":2,\"SECOND_FIELD\":3}}">>)},
     {"Nested record settings",
      ?_encode_decode(
         fun(Rec) ->
                 ?encode_gen(#outer{}, Rec,
                             [{name_handler,
                               {{record,inner}, as_is}}])
         end,
         fun(Bin) ->
                 ?decode_gen(#outer{}, Bin,
                             [{name_handler,
                               {{record,inner}, as_is}}])
         end,
         Input,
         <<"{\"FIELD\":1,"
           "\"NESTED_RECORD\":{"
           "\"first_field\":2,\"second_field\":3}}">>)}
    ].
