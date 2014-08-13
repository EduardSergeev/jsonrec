%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2013, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 12 Jan 2013 by Eduard Sergeev
%%%-------------------------------------------------------------------
-module(jsonrec_nameconv_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/jsonrec.hrl").

-define(encode_decode(Encode, Decode, Rec, Expected, Opts),
        begin
            Bin = list_to_binary(Encode(Rec)),
            case lists:member(debugVal, Opts) of
                true->
                    ?debugFmt("~s~n", [Bin]);
                false ->
                    ok
            end,
            ?assertEqual(Expected, Bin),
            ?assertMatch({ok, Rec}, Decode(Bin))
        end).
-define(encode_decode(Encode, Decode, Rec, Expected),
        ?encode_decode(Encode, Decode, Rec, Expected, [])).
-define(_encode_decode(Encode, Decode, Rec),
        ?_test(?encode_decode(Encode, Decode, Rec))).


-record(rec1,
        {field = 1 :: integer(),
         another_field = 2 :: integer(),
         yet_another_field = 3 :: integer(),
         'ALL_UPPER_FIELD' = 4 :: integer(),
         '_underscore_field' = 5 :: integer()}).


encode_as_is(Rec) ->
    ?encode_gen(#rec1{}, Rec).

decode_as_is(Bin) ->
    ?decode_gen(#rec1{}, Bin).

as_is_test() ->
    ?encode_decode(
       fun encode_as_is/1,
       fun decode_as_is/1,
       #rec1{},
       <<"{"
         "\"field\":1,",
         "\"another_field\":2,"
         "\"yet_another_field\":3,"
         "\"ALL_UPPER_FIELD\":4,"
         "\"_underscore_field\":5"
         "}">>).


encode_camel(Rec) ->
    ?encode_gen(
       #rec1{}, Rec,
       [{name_handler, fun jsonrec:atom_to_camel/1}]).

decode_camel(Bin) ->
    ?decode_gen(
       #rec1{}, Bin,
       [{name_handler, fun jsonrec:atom_to_camel/1}]).

camel_test() ->
    ?encode_decode(
       fun encode_camel/1,
       fun decode_camel/1,
       #rec1{},
       <<"{"
         "\"field\":1,",
         "\"anotherField\":2,"
         "\"yetAnotherField\":3,"
         "\"allUpperField\":4,"
         "\"UnderscoreField\":5"
         "}">>).


encode_pascal(Rec) ->
    ?encode_gen(
       #rec1{}, Rec,
       [{name_handler, fun jsonrec:atom_to_pascal/1}]).

decode_pascal(Bin) ->
    ?decode_gen(
       #rec1{}, Bin,
       [{name_handler, fun jsonrec:atom_to_pascal/1}]).

pascal_test() ->
    ?encode_decode(
       fun encode_pascal/1,
       fun decode_pascal/1,
       #rec1{},
       <<"{"
         "\"Field\":1,",
         "\"AnotherField\":2,"
         "\"YetAnotherField\":3,"
         "\"AllUpperField\":4,"
         "\"UnderscoreField\":5"
         "}">>).
