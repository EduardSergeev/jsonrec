%%%-------------------------------------------------------------------
%%% @author  Eduard Sergeev <eduard.sergeev@amnesty.org.au>
%%% @copyright (C) 2011, Amnesty International Australia
%%% @doc
%%%
%%% @end
%%% Created : 21 Feb 2012 by <eduard.sergeev@amnesty.org.au>
%%%-------------------------------------------------------------------
-module(proper_utils).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([module_par/1, module_par/2]).

module_par(Mod) ->
    module_par(Mod, 120).

module_par(Mod, Timeout) ->
    Ps = [F || {F,_} <- Mod:module_info(exports),
	       lists:prefix("prop_", atom_to_list(F))],
       {"PropEr tests in module " ++ atom_to_list(Mod),
	{inparallel,
	 [{timeout,
	   Timeout,
	   {atom_to_list(Mod) ++ ":" ++ atom_to_list(P),
	    ?_assert(proper:quickcheck(Mod:P()))}}
	  || P <- Ps]}}.
    
