%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2013, Eduard Sergeev
%%% @doc
%%% Common function for `jsonrec' code_generators
%%% @end
%%% Created : 17 Jul 2012 by <eduard.sergeev@gmail.com>
%%%-------------------------------------------------------------------
-module(jsonrec).

-export([atom_to_pascal/1, atom_to_camel/1]).

%%--------------------------------------------------------------------
%% @doc
%% Converts `erlang_atom' into `PascalStyle' string
%% 
%% Convert {@type atom()} is a "standard" Erlang form (`_' separated)
%% into {@type string()} capitalized in "Pascal"-style
%%
%% This function can be specified in {@type jsonrec_encode:name_handler()}
%% or {@type jsonrec_decode:name_handler()} to force generated JSON
%% to use PascalStyle field names
%%
%% Examples:
%% <ul>
%% <li>`atom' -> "Atom"</li>
%% <li>`another_atom' -> "AnotherAtom"</li>
%% <li>`very_verbose_atom' -> "VeryVerboseAtom"</li>
%% </ul>
%% @end
%%--------------------------------------------------------------------
-spec atom_to_pascal(atom()) -> string().
atom_to_pascal(Atom) ->
    List = atom_to_list(Atom),
    Parts = string:tokens(List, "_"),
    Capitalized = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Parts),
    lists:concat(Capitalized).


%%--------------------------------------------------------------------
%% @doc
%% Converts `erlang_atom' into `camelStyle' string
%% 
%% Convert {@type atom()} is a "standard" Erlang form (`_' separated)
%% into {@type string()} capitalized in "Camel"-style
%%
%% This function can be specified in {@type jsonrec_encode:name_handler()}
%% or {@type jsonrec_decode:name_handler()} to force generated JSON
%% to use camelStyle field names
%%
%% Examples:
%% <ul>
%% <li>`atom' -> "atom"</li>
%% <li>`another_atom' -> "anotherAtom"</li>
%% <li>`very_verbose_atom' -> "veryVerboseAtom"</li>
%% </ul>
%% @end
%%--------------------------------------------------------------------
-spec atom_to_camel(atom()) -> string().
atom_to_camel(Atom) ->
    List = atom_to_list(Atom),
    [P|Ps] = string:tokens(List, "_"),
    Cs = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Ps),
    lists:concat([P|Cs]).
