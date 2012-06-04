-module(jsonrecord2).

-include("meta.hrl").
-include("meta_syntax.hrl").

-export([encode_gen_ms/2,
         decode_gen_ms/2]).


encode_gen_ms(Rec, {_Name, Fs}) ->
    gen_encode_ms(Rec, Fs).

decode_gen_ms(Binary, {Name, Fs}) ->
    gen_decode_ms(Binary, Name, Fs).



%%
%% Encoding
%%
gen_encode_ms(Rec, Fs) ->
    EFs = encode_fields_ms(Fs, Rec),
    meta:quote(mochijson2:encode({struct, EFs})).

encode_fields_ms(Fs, Record) ->
    NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
    Es = [encode_field(atom_to_msbinary(Fn), I, Record) ||
             {I, {_, _, #atom{name = Fn}}} <- NFs],
    Cons = fun(H,T) -> meta:quote(H(T)) end,
    lists:foldr(Cons, meta:quote([]), Es).

encode_field(FN, Ind, Rec) ->
    AFN = erl_parse:abstract(FN),
    AInd = erl_parse:abstract(Ind),
    meta:quote(
      fun(Acc) ->
          V = element(AInd, Rec),
          if 
              V =:= undefined ->
                  Acc;
              true ->
                  [{AFN, V}|Acc]
          end
      end).


%%
%% Decoding
%%
gen_decode_ms(Js, Name, Fields) ->
    Size = length(Fields) + 1,
    AS = erl_parse:abstract(Size),
    FTI = gen_field_to_integer(Fields),
    AN = erl_parse:abstract(Name),
    meta:quote(
      begin
          FieldToI = FTI,
          case mochijson2:decode(Js) of
              {struct, Fs} ->
                  Fs1 = [{FieldToI(F), V} || {F,V} <- Fs],
                  Fs2 = [{1,AN} | [{I,V} || {I,V} <- Fs1, is_integer(I)]],
                  erlang:make_tuple(AS, undefined, Fs2)
          end
      end).

gen_field_to_integer(Fs) ->
    NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
    Es = [decode_field(atom_to_msbinary(Fn), I) ||
             {I, {_, _, #atom{name = Fn}}} <- NFs],
    Last = erl_syntax:clause(
             [erl_syntax:underscore()],
             none,
             [meta:quote(undefined)]),
    Es1 = Es ++ [Last],
    Ast = erl_syntax:fun_expr(Es1),
    erl_syntax:revert(Ast).
    

decode_field(FieldName, Index) ->
    AFN = erl_parse:abstract(FieldName),
    AInd = erl_parse:abstract(Index),
    erl_syntax:clause([AFN], none, [AInd]).
    
             
%%
%% Utils
%%
atom_to_mslist(Atom) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    Parts = string:tokens(List, "_"),
    Capitalized = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Parts),
    lists:concat(Capitalized). 

atom_to_msbinary(Atom) ->
    list_to_binary(atom_to_mslist(Atom)).