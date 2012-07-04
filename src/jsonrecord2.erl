-module(jsonrecord2).

-include_lib("meta/include/meta.hrl").
-include_lib("meta/include/meta_syntax.hrl").

-export([encode_gen/3]).

-export([format_error/1]).

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


encode_gen(QRec, {{record,RecordName},_,[]}, Info) ->
    Type = {record, [{atom,0,RecordName}]},
    {Fun,Mps} = gen_encode(QRec, Type, Info, []),
    Fs = [meta:quote(meta:splice(FN) = meta:splice(Def))
          || {_,{FN,Def}} <- lists:reverse(Mps)],
    erl_syntax:block_expr(Fs ++ [Fun(QRec)]).
    

%%
%% Encoding
%%
fetch_encode(QRec, Type, Info, Mps) ->
    case proplists:lookup(Type, Mps) of
        {Type, {Fun,_Def}} ->
            {Fun, Mps};
        none ->
            gen_encode(QRec, Type, Info, Mps)
    end.
    
gen_encode(QRec, {record, [{atom, _, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    QRec1 = gen_var(QRec),
    {Def, Mps1} = encode_fields(QRec1, Fields, Info, Mps),
    Def1 = meta:quote(
             fun(meta:splice(QRec1)) ->
                     {struct, meta:splice(Def)}
             end),
    add_fun_def(Type, Def1, Mps1);
gen_encode(QRec, {list, [{type, _, Type, Args}]}, Info, Mps) ->
    {Fun, Mps1} = fetch_encode(QRec, {Type, Args}, Info, Mps),
    QRec1 = gen_var(QRec),
    Def = meta:quote(
            fun(meta:splice(QRec1)) ->
                    [meta:splice(Fun(meta:quote(X))) || X <- meta:splice(QRec1)]
            end),
    add_fun_def(Type, Def, Mps1);
gen_encode(QRec, {union, [{atom, _, undefined}, {type, _, Type, Args}]}, Info, Mps) ->
    fetch_encode(QRec, {Type, Args}, Info, Mps);

gen_encode(_, {integer, []}, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};
gen_encode(_, {binary, []}, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};
gen_encode(_, {float, []}, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};
gen_encode(_, {boolean, []}, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};
gen_encode(_, undefined, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};

gen_encode(_, Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type, Type).

encode_fields(QRec, Fields, Info, Mps) ->
    NFs = lists:zip(lists:seq(2, length(Fields)+1), Fields),
    {Es,Mps1} = lists:mapfoldl(
                fun({I,T}, M) ->
                        encode_field(QRec, I, T, Info, M)
                end, Mps, NFs),
    Cons = fun(H,T) ->
                   meta:quote((meta:splice(H))(meta:splice(T)))
           end,
    {lists:foldr(Cons, meta:quote([]), Es), Mps1}.

encode_field(QRec, Ind, ?FIELD(Fn), Info, Mps) ->
    encode_typed(QRec, Ind, Fn, undefined, Info, Mps);
 encode_field(QRec, Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    encode_typed(QRec, Ind, Fn, undefined, Info, Mps);
encode_field(QRec, Ind, ?TYPED_FIELD(Fn, T, Args), Info, Mps) ->
    encode_typed(QRec, Ind, Fn, {T,Args}, Info, Mps);
encode_field(QRec, Ind, ?TYPED_FIELD(Fn, T, Args, _Def), Info, Mps) ->
    encode_typed(QRec, Ind, Fn, {T,Args}, Info, Mps).

encode_typed(QRec, Ind, Fn, Type, Info, Mps) ->
    {Fun, Mps1} = fetch_encode(QRec, Type, Info, Mps),
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
    QInd = erl_parse:abstract(Ind),
    AV = meta:quote(V),
    Elem = Fun(AV),
    Def = meta:quote(
            fun(Acc) ->
                    V = element(meta:splice(QInd), meta:splice(QRec)),
                    if 
                        V =:= undefined ->
                            Acc;
                        true ->
                            [{meta:splice(AFN), meta:splice(Elem)}|Acc]
                    end
            end),
    {Def, Mps1}.


%%
%% Decoding
%%
%%gen_decode()

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

gen_var(QRec) ->
    Vn = erl_syntax:variable_name(QRec),
    SVn = atom_to_list(Vn),
    SVn1 = SVn ++ "1",
    erl_syntax:revert(erl_syntax:variable(SVn1)).


%%
%% Formats error messages for compiler 
%%
format_error({unexpected_type, Type}) ->
    format("JSON generator doesn't know how to handle type ~p", [Type]).

format(Format, Args) ->
    io_lib:format(Format, Args).
