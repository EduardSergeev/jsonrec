-module(jsonrecord2).

-include_lib("meta/include/meta.hrl").
-include_lib("meta/include/meta_syntax.hrl").

-export([encode_gen/3, decode_gen/3]).

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
    Fs = [?q(?s(FN) = ?s(Def))
          || {_,{FN,Def}} <- lists:reverse(Mps)],
    erl_syntax:block_expr(Fs ++ [Fun(QRec)]).


decode_gen(QRec, {{record,RecordName},_,[]}, Info) ->
    Type = {record, [{atom,0,RecordName}]},
    {Fun,Mps} = gen_decode(QRec, Type, Info, []),
    Fs = [?q(?s(FN) = ?s(Def))
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
    Def1 = ?q(fun(?s(QRec1)) ->
                      {struct, ?s(Def)}
              end),
    add_fun_def(Type, Def1, Mps1);
gen_encode(QRec, {list, [{type, _, Type, Args}]}, Info, Mps) ->
    {Fun, Mps1} = fetch_encode(QRec, {Type, Args}, Info, Mps),
    QRec1 = gen_var(QRec),
    Def = ?q(fun(?s(QRec1)) ->
                     [?s(Fun(?q(X))) || X <- ?s(QRec1)]
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
                   ?q(?s(H)(?s(T)))
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
    Def = ?q(fun(Acc) ->
                     V = element(?s(QInd), ?s(QRec)),
                     if 
                         V =:= undefined ->
                             Acc;
                         true ->
                             [{?s(AFN), ?s(Fun(?q(V)))} | Acc]
                     end
             end),
    {Def, Mps1}.


%%
%% Decoding
%%
fetch_decode(QRec, Type, Info, Mps) ->
    case proplists:lookup(Type, Mps) of
        {Type, {Fun,_Def}} ->
            {Fun, Mps};
        none ->
            gen_decode(QRec, Type, Info, Mps)
    end.

gen_decode(QStr, {record, [{atom, _, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    {FTI, Mps1} = gen_field_to_integer(QStr, Fields, Info, Mps),
    Size = erl_parse:abstract(length(Fields) + 1),
    QFs = gen_var(QStr),
    Def = ?q(fun({struct, ?s(QFs)}) ->
                     Fs1 = [?s(FTI)(F,V) || {F,V} <- ?s(QFs)],
                     Fs2 = [T || T <- Fs1, is_tuple(T)],
                     erlang:make_tuple(
                       ?s(Size),
                       undefined,
                       ?s(with_defaults(RecName, Fields, ?q(Fs2))))
             end),
    add_fun_def(Type, Def, Mps1);    

gen_decode(QStr, {list, [{type, _, Type, Args}]}, Info, Mps) ->
    {Fun, Mps1} = fetch_decode(QStr, {Type, Args}, Info, Mps),
    QXs = gen_var(QStr),
    Def = ?q(fun(?s(QXs)) ->
                     [?s(Fun(?q(X))) || X <- ?s(QXs)]
             end),
    add_fun_def(Type, Def, Mps1);

gen_decode(QStr, {union, [{atom, _, undefined}, {type, _, Type, Args}]}, Info, Mps) ->
    fetch_decode(QStr, {Type, Args}, Info, Mps);

gen_decode(_, {integer, []}, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};
gen_decode(_, {binary, []}, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};
gen_decode(_, {float, []}, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};
gen_decode(_, {boolean, []}, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};
gen_decode(_, undefined, _Info, Mps) ->
    {fun(Item) -> Item end, Mps};

gen_decode(_, Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type, Type).



gen_field_to_integer(QStr, Types, Info, Mps) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    {Es,Mps1} = lists:mapfoldl(
                  fun({N,T},M) ->
                          decode_field(QStr, N,T, Info, M)
                  end, Mps, NTs),
    [Last] = erl_syntax:fun_expr_clauses(
               ?q(fun(_,_) -> undefined end)),
    Es1 = Es ++ [Last],
    Ast = erl_syntax:fun_expr(Es1),
    {erl_syntax:revert(Ast), Mps1}.

with_defaults(RecName, Types, Tail) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    Ds = [decode_default(N, QDef)
          || {N, ?TYPED_FIELD(_, _, _, QDef)} <- NTs],
    QName = erl_parse:abstract(RecName),
    Tag = ?q({1,?s(QName)}),
    Ast = erl_syntax:list([Tag|Ds], Tail),
    erl_syntax:revert(Ast). 

decode_default(Ind, QDef) ->
    QInd = erl_parse:abstract(Ind),
    ?q({?s(QInd), ?s(QDef)}).


decode_field(QStr, Ind, ?FIELD(Fn), Info, Mps) ->
    decode_record(QStr, Ind, Fn, undefined, Info, Mps);
decode_field(QStr, Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    decode_record(QStr, Ind, Fn, undefined, Info, Mps);
decode_field(QStr, Ind, ?TYPED_FIELD(Fn, T, Args), Info, Mps) ->
    decode_record(QStr, Ind, Fn, {T,Args}, Info, Mps);
decode_field(QStr, Ind, ?TYPED_FIELD(Fn, T, Args, _Def), Info, Mps) ->
    decode_record(QStr, Ind, Fn, {T,Args}, Info, Mps).

decode_record(QStr, Index, Fn, Type, Info, Mps) ->
    {Fun, Mps1} = fetch_decode(QStr, Type, Info, Mps),
    QFn = erl_parse:abstract(atom_to_msbinary(Fn)),
    QInd = erl_parse:abstract(Index),
    [Def] = erl_syntax:fun_expr_clauses(
              ?q(fun(?s(QFn), V1) ->
                         {?s(QInd), ?s(Fun(?q(V1)))}
                 end)),
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
    QFun = erl_syntax:revert(erl_syntax:variable(VN)),
    Fun = fun(Item) ->
                  ?q(?s(QFun)(?s(Item)))
          end,
    {Fun, [{Type,{QFun,Def}}|Mps]}.

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
