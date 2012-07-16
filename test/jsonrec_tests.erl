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

-record(rec0,
        {id :: integer()}).


decode(rec0, Struct) ->
    ?decode_gen(#rec0{}, Struct).
