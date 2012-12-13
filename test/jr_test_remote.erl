%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2012, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 13 Jul 2012 by Eduard Sergeev
%%%-------------------------------------------------------------------
-module(jr_test_remote).

-export([parse_transform/2]).

-export([to_upper/1, to_upper_string/1]).

parse_transform(Forms, _Options) ->
    Forms.

to_upper(Atom) ->
    Str = atom_to_list(Atom),
    UStr = string:to_upper(Str),
    list_to_binary(UStr).

to_upper_string(Atom) ->
    Str = atom_to_list(Atom),
    string:to_upper(Str).

