-module(jsonrecord2).

-include("meta.hrl").
-include("meta_syntax.hrl").

-export([encode_gen/1,
         encode_gen_ms/1,
         decode_gen_ms/1]).


encode_gen({Name, Fs}) ->
    Fun = gen_encode(Name, Fs),
    meta:make_function(encode, Fun).

encode_gen_ms({Name, Fs}) ->
    Fun = gen_encode_ms(Name, Fs),
    meta:make_function(encode, Fun).


decode_gen_ms({Name, Fs}) ->
    Fun = gen_decode_ms(Name, Fs),
    meta:make_function(decode, Fun).



encode_fields(Fs, Record) ->
    NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
    Es = [encode_field(Fn, I, Record) ||
             {I, {_, _, #atom{name = Fn}}} <- NFs],
    Cons = fun(H,T) -> meta:quote(H(T)) end,
    lists:foldr(Cons, meta:quote([]), Es).

encode_fields(Fs, Record, Conv) ->
    NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
    Es = [encode_field(Fn, I, Record, Conv) ||
             {I, {_, _, #atom{name = Fn}}} <- NFs],
    Cons = fun(H,T) -> meta:quote(H(T)) end,
    lists:foldr(Cons, meta:quote([]), Es).

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

encode_field(FN, Ind, Rec, Conv) ->
    AFN = erl_parse:abstract(FN),
    AInd = erl_parse:abstract(Ind),
    meta:quote(
      fun(Acc) ->
          V = element(AInd, Rec),
          if 
              V =:= undefined ->
                  Acc;
              true ->
                  [{Conv(AFN), V}|Acc]
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

%% gen_encode(Name, Fs) ->
%%     Var = meta:quote(Rec),
%%     Record = {record, 0, Name, []},
%%     Pat = meta:quote(Record = Var),
%%     EFs = encode_fields(Fs, Var),
%%     meta:quote(
%%       fun(Pat) ->
%%               mochijson2:encode(
%%                 {struct, EFs})
%%       end).
    
gen_encode(Name, Fs) ->
    Var = meta:quote(Rec),
    Record = {record, 0, Name, []},
    Conv = meta:quote(FieldNameConverter),
    Pat = meta:quote(Record = Var),
    EFs = encode_fields(Fs, Var, Conv),
    meta:quote(
      fun(Conv, Pat) ->
              mochijson2:encode(
                {struct, EFs})
      end).

gen_encode_ms(Name, Fs) ->
    Var = meta:quote(Rec),
    Record = {record, 0, Name, []},
    Pat = meta:quote(Record = Var),
    EFs = encode_fields_ms(Fs, Var),
    meta:quote(
      fun(Pat) ->
              mochijson2:encode(
                {struct, EFs})
      end).


gen_decode_ms(Name, Fields) ->
    Size = length(Fields) + 1,
    AS = erl_parse:abstract(Size),
    FTI = gen_field_to_integer(Fields),
    AN = erl_parse:abstract(Name),
    meta:quote(
      fun(Js) ->
              FieldToI = FTI,
              case mochijson2:decode(Js) of
                  {struct, Fs} ->
                      Fs1 = [{FieldToI(F), V} || {F,V} <- Fs],
                      Fs2 = [{1,AN} | [{I,V} || {I,V} <- Fs1, is_integer(I)]],
                      erlang:make_tuple(AS, undefined, Fs2)
              end
      end).
    
    
             
%%
%% Utils
%%
atom_to_mslist(Atom) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    Parts = string:tokens(List, "_"),
    Capitalized = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Parts),
    lists:concat(Capitalized). 

mslist_to_atom([F|Cs]) when (F >= $A) and (F =< $Z) ->
    Rs = lists:flatmap(
	   fun(C) when (C >= $A) and (C =< $Z) -> "_" ++ string:to_lower([C]);
	      (C) -> [C]
	   end,
	   Cs),
    list_to_atom(string:to_lower([F]) ++ Rs).


atom_to_msbinary(Atom) ->
    list_to_binary(atom_to_mslist(Atom)).

msbinary_to_atom(Binary) ->
    mslist_to_atom(binary_to_list(Binary)).
