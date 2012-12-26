-module(jsonrec).


-export([atom_to_pascal/1, atom_to_camel/1]).


atom_to_pascal(Atom) ->
    List = atom_to_list(Atom),
    Parts = string:tokens(List, "_"),
    Capitalized = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Parts),
    lists:concat(Capitalized).

atom_to_camel(Atom) ->
    List = atom_to_list(Atom),
    [P|Ps] = string:tokens(List, "_"),
    Cs = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Ps),
    lists:concat([P|Cs]).
