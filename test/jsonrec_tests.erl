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

-record(rec0,
        {id :: integer(),
         status = new :: status(),
         atom :: atom(),
         binary :: binary()}).


decode(rec0, Struct) ->
    ?decode_gen(#rec0{}, Struct).

encode(#rec0{} = _Rec) ->
    ok.


decode_test_() ->
    [{"Simple decode",
      begin
          Inp = <<"{
                    \"Id\":42,
                    \"Atom\" : \"status\",
                    \"Binary\"   :\"Some string\"
                   }">>,
          ?_assertMatch(
             {#rec0{id = 42, status = new,
                    atom = status, binary = <<"Some string">>},
              <<>>},
             decode(rec0, Inp))
      end}].
