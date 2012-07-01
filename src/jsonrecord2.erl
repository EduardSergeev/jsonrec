-module(jsonrecord2).

-include("meta.hrl").
-include("meta_syntax.hrl").

-export([encode_gen/3]).

-define(FIELD(Name),
        {record_field, _Ln1,
         {atom, _Ln2, Name}}).
-define(FIELD(Name, Default),
        {record_field, _Ln1,
         {atom, _Ln2, Name},
         Default}).
-define(TYPED_FIELD(Name, Type, Args),
        {typed_record_field,
         ?FIELD(Name),
         {type, _Ln3, Type, Args}}).
-define(TYPED_FIELD(Name, Type, Args, Default),
        {typed_record_field,
         ?FIELD(Name, Default),
         {type, _Ln3, Type, Args}}).


encode_gen(Item, {{record,RecordName},_,[]}, Info) ->
    Type = {record, [{atom,0,RecordName}]},
    {Fun,Mps} = gen_encode(Type, Info, []),
    Fs = lists:map(
           fun({_,{FN,Def}}) ->
                   meta:quote(meta:splice(FN) = meta:splice(Def))
           end,
           lists:reverse(Mps)),
    erl_syntax:block_expr(Fs ++ [Fun(Item)]).
    

%%
%% Encoding
%%
fetch_encode(Type, Info, Mps) ->
    case proplists:lookup(Type, Mps) of
        {Type, {Fun,_Def}} ->
            {Fun, Mps};
        none ->
            gen_encode(Type, Info, Mps)
    end.
    
gen_encode({record, [{atom, _, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    {FDef, Mps1} = encode_fields(Fields, Info, Mps),
    Def1 = meta:quote(
             fun(Rec) ->
                     {struct, meta:splice(FDef(meta:quote(Rec)))}
             end),
    add_fun_def(Type, Def1, Mps1);
gen_encode({list, [{type, _, Type, Args}]}, Info, Mps) ->
    {Fun, Mps1} = fetch_encode({Type, Args}, Info, Mps),
    AX = meta:quote(X),
    Def = meta:quote(
            fun(Xs) ->
                    [meta:splice(Fun(AX)) || X <- Xs]
            end),
    add_fun_def(Type, Def, Mps1);
gen_encode({union, [{atom, _, undefined}, {type, _, Type, Args}]}, Info, Mps) ->
    fetch_encode({Type, Args}, Info, Mps);

gen_encode({integer, []}, _Info, Mps) ->
    Fun = fun(Item) -> Item end,
    {Fun, Mps};
gen_encode({binary, []}, _Info, Mps) ->
    Fun = fun(Item) -> Item end,
    {Fun, Mps};
gen_encode({float, []}, _Info, Mps) ->
    Fun = fun(Item) -> Item end,
    {Fun, Mps};
gen_encode(Type, _Info, _Mps) ->
    meta:meta_error({unexpected_type, Type}).

encode_fields(Fields, Info, Mps) ->
    NFs = lists:zip(lists:seq(2, length(Fields)+1), Fields),
    {Es,Mps1} = lists:mapfoldl(
                fun({I,T}, M) ->
                        encode_field(I, T, Info, M)
                end,
                Mps,
                NFs),
    Cons = fun(H,T) ->
                   meta:quote((meta:splice(H))(meta:splice(T)))
           end,
    {fun(Rec) ->
             Es1 = [E(Rec) || E <- Es],
             lists:foldr(Cons, meta:quote([]), Es1)
     end,
     Mps1}.

encode_field(Ind, ?FIELD(Fn), Info, Mps) ->
    encode_typed(Ind, Fn, undefined, Info, Mps);
 encode_field(Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    encode_typed(Ind, Fn, undefined, Info, Mps);
encode_field(Ind, ?TYPED_FIELD(Fn, T, Args), Info, Mps) ->
    encode_typed(Ind, Fn, {T,Args}, Info, Mps);
encode_field(Ind, ?TYPED_FIELD(Fn, T, Args, _Def), Info, Mps) ->
    encode_typed(Ind, Fn, {T,Args}, Info, Mps).

encode_typed(Ind, Fn, Type, Info, Mps) ->
    {Fun, Mps1} = fetch_encode(Type, Info, Mps),
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
    AInd = erl_parse:abstract(Ind),
    AV = meta:quote(V),
    Elem = Fun(AV),
    Def = fun(Rec) ->
                  meta:quote(
                    fun(Acc) ->
                            V = element(meta:splice(AInd), meta:splice(Rec)),
                            if 
                                V =:= undefined ->
                                    Acc;
                                true ->
                                    [{meta:splice(AFN), meta:splice(Elem)}|Acc]
                            end
                    end)
          end,
    {Def, Mps1}.
             
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


add_fun_def(Type, Def, Mps) ->
    Ind = length(Mps),
    VN = list_to_atom("Fun" ++ integer_to_list(Ind)),
    AFun = erl_syntax:revert(erl_syntax:variable(VN)),
    Fun = fun(Item) ->
                  meta:quote((meta:splice(AFun))(meta:splice(Item)))
          end,
    {Fun, [{Type,{AFun,Def}}|Mps]}.
