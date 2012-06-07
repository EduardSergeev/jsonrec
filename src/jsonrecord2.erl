-module(jsonrecord2).

-include("meta.hrl").
-include("meta_syntax.hrl").

-export([encode_gen_ms/2,
         decode_gen_ms/2,

         encode_gen_ms/4,
         decode_gen_ms/4]).


encode_gen_ms(Rec, {_Name, Fields}) ->
    EFs = encode_fields_ms(Fields, Rec),
    meta:quote(mochijson2:encode({struct, EFs})).

encode_gen_ms(Rec,
               {Name, Fields},
               {{record, Name}, TypeDefs, []},
               Mps) ->
    EFs = encode_fields_ms2(Rec, Fields, TypeDefs, Mps),
    meta:quote({struct, EFs}).

decode_gen_ms(Js, {Name, Fields}) ->
    Size = length(Fields) + 1,
    AS = erl_parse:abstract(Size),
    FTI = gen_field_to_integer(Fields),
    AN = erl_parse:abstract(Name),
    meta:quote(
      case mochijson2:decode(Js) of
          {struct, Fs} ->
              Fs1 = [{FTI(F), V} || {F,V} <- Fs],
              Fs2 = [{1,AN} | [{I,V} || {I,V} <- Fs1, is_integer(I)]],
              erlang:make_tuple(AS, undefined, Fs2)
      end).

%% decode_gen_ms(Struct,
%%                {Name, Fields},
%%                {{record, Name}, TypeDefs, []},
%%                Mps) ->
%%     Size = length(Fields) + 1,
%%     AS = erl_parse:abstract(Size),
%%     Ds = with_defaults(Name, Fields),
%%     FTI = gen_field_to_integer(Fields, TypeDefs, Mps),
%%     meta:quote(
%%       case Struct of
%%           {struct, Fs} ->
%%               Fs1 = [FTI(F,V) || {F,V} <- Fs],
%%               Fs2 = Ds ++ [T || T <- Fs1, is_tuple(T)],
%%               erlang:make_tuple(AS, undefined, Fs2)
%%       end).

decode_gen_ms(Struct,
               {Name, Fields},
               {{record, Name}, TypeDefs, []},
               Mps) ->
    Size = length(Fields) + 1,
    AS = erl_parse:abstract(Size),
    FTI = gen_field_to_integer(Fields, TypeDefs, Mps),
    AFs = meta:quote(Fs),
    AFs1 = meta:quote([FTI(F,V) || {F,V} <- AFs]),
    AFs2 = meta:quote([T || T <- AFs1, is_tuple(T)]),
    AFs3 = with_defaults(Name, Fields, AFs2),
    meta:quote(
      case Struct of
          {struct, AFs} ->
              erlang:make_tuple(AS, undefined, AFs3)
      end).



%%
%% Encoding
%%
encode_fields_ms(Fs, Record) ->
    NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
    Es = [encode_field(atom_to_msbinary(Fn), I, Record) ||
             {I, {_, _, #atom{name = Fn}}} <- NFs],
    Cons = fun(H,T) -> meta:quote(H(T)) end,
    lists:foldr(Cons, meta:quote([]), Es).

encode_fields_ms2(Record, Fields, Types, Mps) ->
    NFs = lists:zip3(lists:seq(2, length(Fields)+1), Fields, Types),
    Es = [encode_field2(I, F, T, Record, Mps) ||
             {I, F, T} <- NFs],
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

encode_field2(Ind, {record_field, _Ln, #atom{name = Fn}} = RF,
              RF, Rec, _) ->
    encode_plain(Ind, Fn, Rec);
encode_field2(Ind, {record_field, _Ln, #atom{name = Fn}, _Def} = RF,
              RF, Rec, _) ->
    encode_plain(Ind, Fn, Rec);

encode_field2(Ind, {record_field, _Ln, #atom{name = Fn}} = RF,
              {typed_record_field, RF, T}, Rec, Mps) ->
    encode_typed(Ind, RF, Fn, T, Rec, Mps);
encode_field2(Ind, {record_field, _Ln, #atom{name = Fn}, _Def} = RF,
              {typed_record_field, RF, T}, Rec, Mps) ->
    encode_typed(Ind, RF, Fn, T, Rec, Mps).

encode_plain(Ind, Fn, Rec) ->            
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
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

encode_typed(Ind, RF, Fn, T, Rec, Mps) ->
    case T of
        {type, _, union,
         [_Und, {type, _, record, [{atom, _, Name}]}]} ->
            Fun = get_fun(Name, Mps),
            encode_record(Ind, Fn, Fun, Rec);
        {type, _, record,
         [{atom, _, Name}]} ->
            Fun = get_fun(Name, Mps),
            encode_record(Ind, Fn, Fun, Rec);
        {type, _, list,
         [{type, _, record, [{atom, _, Name}]}]} ->
            Fun = get_fun(Name, Mps),
            encode_list(Ind, Fn, Fun, Rec);
        {type, _, union,
         [_Und,
          {type, _, list,
           [{type, _, record, [{atom, _, Name}]}]}]} ->
            Fun = get_fun(Name, Mps),
            encode_list(Ind, Fn, Fun, Rec);
        _ ->
            encode_field2(Ind, RF, RF, Rec, Mps)
    end.


encode_record(Ind, Fn, Fun, Rec) ->
    AFun = erl_parse:abstract(Fun),
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
    AInd = erl_parse:abstract(Ind),
    meta:quote(
      fun(Acc) ->
              V = element(AInd, Rec),
              if 
                  V =:= undefined ->
                      Acc;
                  true ->
                      [{AFN, AFun(V)}|Acc]
              end
      end).

encode_list(Ind, Fn, Fun, Rec) ->
    AFun = erl_parse:abstract(Fun),
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
    AInd = erl_parse:abstract(Ind),
    meta:quote(
      fun(Acc) ->
              Vs = element(AInd, Rec),
              if 
                  Vs =:= undefined ->
                      Acc;
                  true ->
                      [{AFN, [AFun(V) || V <- Vs]}|Acc]
              end
      end).





%%
%% Decoding
%%
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


%% gen_defaults(Fs) ->
%%     NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
%%     Ds = [decode_default(N, Def) || {N, {record_field, _, _, Def}} <- NFs],
%%     Ast = erl_syntax:list(Ds),
%%     erl_syntax:revert(Ast).    

%% with_defaults(Fs, Tail) ->
%%     NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
%%     Ast = case [decode_default(N, Def) || {N, {record_field, _, _, Def}} <- NFs] of
%%               [] ->
%%                   Tail;
%%               Ds ->
%%                   erl_syntax:list(Ds, Tail)
%%           end,
%%     erl_syntax:revert(Ast).    

with_defaults(Name, Fs, Tail) ->
    NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
    Ds = [decode_default(N, Def)
          || {N, {record_field, _, _, Def}} <- NFs],
    AN = erl_parse:abstract(Name),
    Tag = meta:quote({1,AN}),
    Ast = erl_syntax:list([Tag|Ds], Tail),
    erl_syntax:revert(Ast).    

gen_field_to_integer(Fields, Types, Mps) ->
    NFTs = lists:zip3(lists:seq(2, length(Fields)+1), Fields, Types),
    Es = [decode_field(N,F,T,Mps) || {N,F,T} <- NFTs],
    Last = erl_syntax:clause(
             [erl_syntax:underscore(),erl_syntax:underscore()],
             none,
             [meta:quote(undefined)]),
    Es1 = Es ++ [Last],
    Ast = erl_syntax:fun_expr(Es1),
    erl_syntax:revert(Ast).

decode_default(Ind, Def) ->
    AInd = erl_parse:abstract(Ind),
    meta:quote({AInd, Def}).

decode_field(Ind, {record_field, _, #atom{name = Fn}} = RF,
             RF, _) ->
    decode_plain(Ind, Fn);
decode_field(Ind,{record_field, _, #atom{name = Fn}, _Def} = RF,
             RF, _) ->
    decode_plain(Ind, Fn);

decode_field(Ind, {_, _, #atom{name = Fn}} = RF,
             {typed_record_field, RF, T}, Mps) ->
    decode_typed(Ind, RF, Fn, T, Mps);
decode_field(Ind, {_, _, #atom{name = Fn}, _Def} = RF,
             {typed_record_field, RF, T}, Mps) ->
    decode_typed(Ind, RF, Fn, T, Mps).

decode_plain(Ind, Fn) ->
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
    Var = meta:quote(V0),
    AInd = erl_parse:abstract(Ind),
    Res = meta:quote({AInd,Var}),
    erl_syntax:clause([AFN, Var], none, [Res]).

decode_typed(Ind, RF, Fn, T, Mps) ->   
    case T of
        {type, _, union,
         [_Und,
          {type, _, record, [{atom, _, Name}]}]} ->
            Fun = get_fun(Name, Mps),
            decode_record(Ind, Fn, Name, Fun);
        {type, _, record,
         [{atom, _, Name}]} ->
            Fun = get_fun(Name, Mps),
            decode_record(Ind, Fn, Name, Fun);
        {type, _, list,
         [{type, _, record, [{atom, _, Name}]}]} ->
            Fun = get_fun(Name, Mps),
            decode_list(Ind, Fn, Name, Fun);
        {type, _, union,
         [_Und,
          {type, _, list,
           [{type, _, record, [{atom, _, Name}]}]}]} ->
            Fun = get_fun(Name, Mps),
            decode_list(Ind, Fn, Name, Fun);
        _ ->
            decode_field(Ind, RF, RF, Mps)
    end.
    

decode_record(Index, Fn, Name, Fun) ->
    AFun = erl_parse:abstract(Fun),
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
    Var = meta:quote(V1),
    AInd = erl_parse:abstract(Index),
    Type = erl_parse:abstract(Name),
    Res = meta:quote({AInd,AFun(Type, Var)}),
    erl_syntax:clause([AFN, Var], none, [Res]).

decode_list(Index, Fn, Name, Fun) ->
    AFun = erl_parse:abstract(Fun),
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
    Var = meta:quote(Vs),
    AInd = erl_parse:abstract(Index),
    Type = erl_parse:abstract(Name),
    Res = meta:quote({AInd,[AFun(Type, V2) || V2 <- Var]}),
    erl_syntax:clause([AFN, Var], none, [Res]).
            

    
get_fun(Name, Mps) when is_list(Mps) ->
    proplist:get_value(Name, Mps);
get_fun(_, FunName) ->
    FunName.

    
             
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
