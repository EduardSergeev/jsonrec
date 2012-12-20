-module(jsonrec).


-export([atom_to_pascal/1]).


atom_to_pascal(Atom) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    Parts = string:tokens(List, "_"),
    Capitalized = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Parts),
    lists:concat(Capitalized).
