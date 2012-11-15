%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2012, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 24 Oct 2012 by Eduard Sergeev
%%%-------------------------------------------------------------------
-module(json_parsers).

-include("mparsers.hrl").

-compile(export_all).


whitespace() ->
    oneof(" \t\n\r").


positive() ->
    do([ list_to_integer([D|Ds]) ||
           D <- range($1, $9),
           Ds <- many(range($0, $9)) ]).

%% positive() ->
%%     Fun = fun(D,N) -> N*10 + (D - $0) end,
%%     do([ N ||
%%            D <- range($1, $9),
%%            N <- many_fold(range($0, $9), Fun, D - $0) ]).

zero() ->
    singleton($0) > return(0).

negative() ->
    singleton($-) > do([ -N || N <- positive() ]).

%% integer() ->
%%     do([ S*I ||
%%            S <- option(singleton($-) > return(-1), 1),
%%            I <- case S of
%%                     1 ->
%%                         either(zero(), positive());
%%                     -1 ->
%%                         positive()
%%                 end ]).

integer() ->
    do([ I ||
           S <- option(singleton($-), undefined),
           I <- case S of
                    $- ->
                        do([ -P || P <- positive() ]);
                    _ ->
                        zero() or positive()
                end ]).


%%
%% tests
%%
int_list() ->
    sep_by(integer(), whitespace()).
    
