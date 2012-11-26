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

-type status() :: new | old | unknown.

%% -record(rec0,
%%          {sta :: status()}).

%% -record(rec0,
%%         {id = 42 :: integer()}).

-record(rec0,
        {id :: integer(),
         status = new :: status(),
         atom :: atom(),
         binary :: binary(),
         boolean :: boolean()}).


%%-type my_integer() :: integer().

-record(rec1,
        {id :: integer(),
         rec0 :: #rec0{}}).


decode(rec0, Bin) ->
    ?decode_gen(#rec0{}, Bin);
decode(rec1, Bin) ->
    ?decode_gen(#rec1{}, Bin).




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
                    \"Rec0\": 
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
                          rec0 = #rec0{id = 2, status = old, atom = status,
                                       boolean = false, binary = <<>>}}},
               decode(rec1, Inp))
        end)}].
