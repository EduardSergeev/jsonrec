-module(jsonrecord2).

-include_lib("meta/include/meta.hrl").
-include_lib("meta/include/meta_syntax.hrl").

-export([encode_gen/3, encode_gen/4,
         decode_gen/3, decode_gen/4]).

-export([format_error/1]).

-define(TYPE(Type, Args),
        {type, _Ln3, Type, Args}).
-define(FIELD(Name),
        {record_field, _Ln1,
         {atom, _Ln2, Name}}).
-define(FIELD(Name, Default),
        {record_field, _Ln1,
         {atom, _Ln2, Name},
         Default}).
-define(TYPED_FIELD(Name, Type),
        {typed_record_field,
         ?FIELD(Name),
         ?TYPE(_Type, _Args) = Type}).
-define(TYPED_FIELD(Name, Type, Default),
        {typed_record_field,
         ?FIELD(Name, Default),
         ?TYPE(_Type, _Args) = Type}).

-record(mps, {defs, subs}).

encode_gen(QRec, Record, Info) ->
    encode_gen(QRec, Record, Info, []).

encode_gen(QRec, {{record,RecordName},_,[]}, Info, Subs) ->
    Type = {record, [{atom,RecordName}]},
    Subs1 =
        [{{record,[{atom,IR}]}, F}
         || {{{record,IR},_,[]},F} <- Subs],
    Mps = #mps{defs = [], subs = Subs1},
    {Fun,Mps1} = gen_encode(QRec, Type, Info, Mps),
    Fs = [?q(?s(FN) = ?s(Def))
          || {_,{_,FN,_GFun,Def}} <- lists:reverse(Mps1#mps.defs),
             Def /= none],
    erl_syntax:block_expr(Fs ++ [Fun(QRec)]).

decode_gen(QRec, Record, Info) ->
    decode_gen(QRec, Record, Info, []).

decode_gen(QRec, {{record,RecordName},_,[]}, Info, Subs) ->
    Type = {record, [{atom,RecordName}]},
    Subs1 =
        [{{record,[{atom,IR}]}, F}
         || {{{record,IR},_,[]},F} <- Subs],
    Mps = #mps{defs = [], subs = Subs1},
    {Fun,Mps1} = gen_decode(QRec, Type, Info, Mps),
    Fs = [?q(?s(FN) = ?s(Def))
          || {_,{_,FN,_GFun,Def}} <- lists:reverse(Mps1#mps.defs),
             Def /= none],
    erl_syntax:block_expr(Fs ++ [Fun(QRec)]).


    

%%
%% Encoding
%%
fetch_encode(QRec, Type, Info, Mps) ->
    case proplists:lookup(Type, Mps#mps.subs) of
        none ->
            case proplists:lookup(Type, Mps#mps.defs) of
                {Type, {Fun, _FN, _GFun, _Def}} ->
                    {Fun, Mps};
                none ->
                    gen_encode(QRec, Type, Info, Mps)
            end;
        {Type, {M,F}} ->
            VFun = fun(_) ->
                           fun(Item) -> 
                                   QM = erl_parse:abstract(M),
                                   QF = erl_parse:abstract(F),
                                   ?q(?s(QM):?s(QF)(?s(Item)))
                           end
                   end,
            add_fun_def(Type, none, Mps, none, VFun);
        {Type, F} ->
            VFun = fun(_) ->
                           fun(Item) -> 
                                   QF = erl_parse:abstract(F),
                                   ?q(?s(QF)(?s(Item)))
                           end
                   end,
            add_fun_def(Type, none, Mps, none, VFun)
    end.
    
gen_encode(QRec, {record, [{atom, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    QRec1 = gen_var(QRec),
    {Def, Mps1} = encode_fields(QRec1, Fields, Info, Mps),
    Def1 = ?q(fun(?s(QRec1)) ->
                      {struct, ?s(Def)}
              end),
    GFun = fun(Item) ->
                   ?q(is_record(?s(Item),?s(erl_parse:abstract(RecName))))
           end,
    add_fun_def(Type, Def1, Mps1, GFun);
gen_encode(QRec, {list, [InnerType]} = Type, Info, Mps) ->
    {Fun, Mps1} = fetch_encode(QRec, type_ref(InnerType), Info, Mps),
    QRec1 = gen_var(QRec),
    Def = ?q(fun(?s(QRec1)) ->
                     [?s(Fun(?q(X))) || X <- ?s(QRec1)]
             end),
    add_fun_def(Type, Def, Mps1);

gen_encode(QRec, {union, Types} = Type, Info, Mps) ->
    {Def,Mps1} = encode_union(QRec, Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_encode(_, {atom, Atom} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           erl_parse:abstract(Atom)
                   end
           end,
    GFun = fun(Item) ->
                   ?q(?s(Item) =:= ?s(erl_parse:abstract(Atom)))
           end,
    add_fun_def(Type, none, Mps, GFun, VFun);

gen_encode(_, {integer, []} = Type, _Info, Mps) ->
    GFun = fun(Item) ->
                   ?q(is_integer(?s(Item)))
           end,
    encode_standard(Type, GFun, Mps);
gen_encode(_, {binary, []} = Type, _Info, Mps) ->
    GFun = fun(Item) ->
                   ?q(is_binary(?s(Item)))
           end,
    encode_standard(Type, GFun, Mps);
gen_encode(_, {float, []} = Type, _Info, Mps) ->
    GFun = fun(Item) ->
                   ?q(is_float(?s(Item)))
           end,
    encode_standard(Type, GFun, Mps);
gen_encode(_, {boolean, []} = Type, _Info, Mps) ->
    GFun = fun(Item) ->
                   ?q(is_boolean(?s(Item)))
           end,
    encode_standard(Type, GFun, Mps);

gen_encode(_, {any, []} = Type, _Info, Mps) ->
    GFun = fun(_Item) ->
                   ?q(true)
           end,
    encode_standard(Type, GFun, Mps);

gen_encode(QRec, {UserType,[]} = Type, Info, Mps) ->
    {_, Type1, []} =  meta:reify_type(UserType, Info),
    TR = type_ref(Type1),
    {Fun, Mps1} = fetch_encode(QRec, TR, Info, Mps),
    VFun = fun(_) -> Fun end,
    GFun = get_guard(TR, Mps1),
    add_fun_def(Type, none, Mps1, GFun, VFun);
            
gen_encode(_QRec, Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type_encode, Type).

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
    encode_typed(QRec, Ind, Fn, {any, []}, Info, Mps);
encode_field(QRec, Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    encode_typed(QRec, Ind, Fn, {any, []}, Info, Mps);
encode_field(QRec, Ind, ?TYPED_FIELD(Fn, Type), Info, Mps) ->
    encode_typed(QRec, Ind, Fn, type_ref(Type), Info, Mps);
encode_field(QRec, Ind, ?TYPED_FIELD(Fn, Type, _Def), Info, Mps) ->
    encode_typed(QRec, Ind, Fn, type_ref(Type), Info, Mps).

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

encode_union(QRec, Types, Info, Mps) ->
    {Cs,MpsN} =
        lists:mapfoldl(
          fun(TA, Mps1) ->
                  Type = type_ref(TA),
                  {VFun, Mps2} = fetch_encode(QRec, Type, Info, Mps1),
                  QF = case get_guard(Type, Mps2) of
                           none ->
                               ?q(fun(V) ->
                                          ?s(VFun(?q(V)))
                                  end);
                           GFun ->
                               ?q(fun(V) when ?s(GFun(?q(V))) ->
                                          ?s(VFun(?q(V)))
                                  end)
                       end,
                  [C] = erl_syntax:fun_expr_clauses(QF),
                  {C,Mps2}
          end, Mps, Types),
    Def = erl_syntax:revert(erl_syntax:fun_expr(Cs)),
    {Def, MpsN}.

encode_standard(Type, GFun, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           Item
                   end
           end,
    add_fun_def(Type, none, Mps, GFun, VFun).

    

%%
%% Decoding
%%
fetch_decode(QRec, Type, Info, Mps) ->
    case proplists:lookup(Type, Mps#mps.subs) of
        none ->
            case proplists:lookup(Type, Mps#mps.defs) of
                {Type, {Fun, _FN, _GFun, _Def}} ->
                    {Fun, Mps};
                none ->
                    gen_decode(QRec, Type, Info, Mps)
            end;
        {Type, {M,F}} ->
            VFun = fun(_) ->
                           fun(Item) -> 
                                   QM = erl_parse:abstract(M),
                                   QF = erl_parse:abstract(F),
                                   ?q(?s(QM):?s(QF)(?s(Item)))
                           end
                   end,
            add_fun_def(Type, none, Mps, none, VFun);
        {Type, F} ->
            VFun = fun(_) ->
                           fun(Item) -> 
                                   QF = erl_parse:abstract(F),
                                   ?q(?s(QF)(?s(Item)))
                           end
                   end,
            add_fun_def(Type, none, Mps, none, VFun)
    end.


gen_decode(QStr, {record, [{atom, RecName}]} = Type, Info, Mps) ->
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

gen_decode(QStr, {list, [InnerType]} = Type, Info, Mps) ->
    {Fun, Mps1} = fetch_decode(QStr, type_ref(InnerType), Info, Mps),
    QXs = gen_var(QStr),
    Def = ?q(fun(?s(QXs)) ->
                     [?s(Fun(?q(X))) || X <- ?s(QXs)]
             end),
    add_fun_def(Type, Def, Mps1);

%% gen_decode(QStr, {union, [{atom, undefined}, Type]}, Info, Mps) ->
%%     fetch_decode(QStr, type_ref(Type), Info, Mps);

gen_decode(QRec, {union, Types} = Type, Info, Mps) ->
    {Def,Mps1} = decode_union(QRec, Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_decode(_, {atom, Atom} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           erl_parse:abstract(Atom)
                   end
           end,
    GFun = fun(Item) ->
                   ?q(?s(Item) =:= ?s(erl_parse:abstract(Atom)))
           end,
    add_fun_def(Type, none, Mps, GFun, VFun);

gen_decode(_, {integer, []} = Type, _Info, Mps) ->
    GFun = fun(Item) ->
                   ?q(is_integer(?s(Item)))
           end,
    decode_standard(Type, GFun, Mps);
gen_decode(_, {binary, []} = Type, _Info, Mps) ->
    GFun = fun(Item) ->
                   ?q(is_binary(?s(Item)))
           end,
    decode_standard(Type, GFun, Mps);
gen_decode(_, {float, []} = Type, _Info, Mps) ->
    GFun = fun(Item) ->
                   ?q(is_float(?s(Item)))
           end,
    decode_standard(Type, GFun, Mps);
gen_decode(_, {boolean, []} = Type, _Info, Mps) ->
    GFun = fun(Item) ->
                   ?q(is_boolean(?s(Item)))
           end,
    decode_standard(Type, GFun, Mps);

gen_decode(_, {any, []} = Type, _Info, Mps) ->
    GFun = fun(_Item) ->
                   ?q(true)
           end,
    decode_standard(Type, GFun, Mps);

gen_decode(QStr, {UserType,[]} = Type, Info, Mps) ->
    {_, Type1, []} =  meta:reify_type(UserType, Info),
    TR = type_ref(Type1),
    {Fun, Mps1} = fetch_decode(QStr, TR, Info, Mps),
    VFun = fun(_) -> Fun end,
    GFun = get_guard(TR, Mps1),
    add_fun_def(Type, none, Mps1, GFun, VFun);
    
gen_decode(_, Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type_decode, Type).



gen_field_to_integer(QStr, Types, Info, Mps) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    {Es,Mps1} = lists:mapfoldl(
                  fun({N,T},M) ->
                          decode_field(QStr, N, T, Info, M)
                  end, Mps, NTs),
    [Last] = erl_syntax:fun_expr_clauses(
               ?q(fun(_,_) -> undefined end)),
    Es1 = Es ++ [Last],
    Ast = erl_syntax:fun_expr(Es1),
    {erl_syntax:revert(Ast), Mps1}.

with_defaults(RecName, Types, Tail) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    Ds = [decode_default(N, QDef)
          || {N, ?TYPED_FIELD(_, _, QDef)} <- NTs],
    QName = erl_parse:abstract(RecName),
    Tag = ?q({1,?s(QName)}),
    Ast = erl_syntax:list([Tag|Ds], Tail),
    erl_syntax:revert(Ast). 

decode_default(Ind, QDef) ->
    QInd = erl_parse:abstract(Ind),
    ?q({?s(QInd), ?s(QDef)}).


decode_field(QStr, Ind, ?FIELD(Fn), Info, Mps) ->
    decode_record(QStr, Ind, Fn, {any, []}, Info, Mps);
decode_field(QStr, Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    decode_record(QStr, Ind, Fn, {any, []}, Info, Mps);
decode_field(QStr, Ind, ?TYPED_FIELD(Fn, Type), Info, Mps) ->
    decode_record(QStr, Ind, Fn, type_ref(Type), Info, Mps);
decode_field(QStr, Ind, ?TYPED_FIELD(Fn, Type, _Def), Info, Mps) ->
    decode_record(QStr, Ind, Fn, type_ref(Type), Info, Mps).

decode_record(QStr, Index, Fn, Type, Info, Mps) ->
    {Fun, Mps1} = fetch_decode(QStr, Type, Info, Mps),
    QFn = erl_parse:abstract(atom_to_msbinary(Fn)),
    QInd = erl_parse:abstract(Index),
    [Def] = erl_syntax:fun_expr_clauses(
              ?q(fun(?s(QFn), V1) ->
                         {?s(QInd), ?s(Fun(?q(V1)))}
                 end)),
    {Def, Mps1}. 

decode_union(QStr, Types, Info, Mps) ->
    {Cs,MpsN} =
        lists:mapfoldl(
          fun(TA, Mps1) ->
                  Type = type_ref(TA),
                  {VFun, Mps2} = fetch_decode(QStr, Type, Info, Mps1),
                  QF = case get_guard(Type, Mps2) of
                           none ->
                               ?q(fun(V) ->
                                          ?s(VFun(?q(V)))
                                  end);
                           GFun ->
                               ?q(fun(V) when ?s(GFun(?q(V))) ->
                                          ?s(VFun(?q(V)))
                                  end)
                       end,
                  [C] = erl_syntax:fun_expr_clauses(QF),
                  {C,Mps2}
          end, Mps, Types),
    Def = erl_syntax:revert(erl_syntax:fun_expr(Cs)),
    {Def, MpsN}.

decode_standard(Type, GFun, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           Item
                   end
           end,
    add_fun_def(Type, none, Mps, GFun, VFun).

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

type_ref({type, _Ln, Tag, Args}) ->
%%    {Tag, Args};
    {Tag, [type_ref(A) || A <- Args]};
type_ref({atom, _Ln, Atom}) ->
    {atom, Atom};
type_ref(Converted) ->
    Converted.


add_fun_def(Type, Def, Mps) ->
    add_fun_def(Type, Def, Mps, none).

add_fun_def(Type, Def, Mps, GFun) ->
    VFun = fun(QFunName) ->
                   fun(Item) ->
                           ?q(?s(QFunName)(?s(Item)))
                   end
           end,
    add_fun_def(Type, Def, Mps, GFun, VFun).

add_fun_def(Type, Def, #mps{defs = Defs} = Mps, GFun, VFun) ->
    Ind = length(Defs),
    FunName = list_to_atom("Fun" ++ integer_to_list(Ind)),
    QVFunName = erl_syntax:revert(erl_syntax:variable(FunName)),
    Fun = VFun(QVFunName),
    Defs1 = [{Type,{Fun,QVFunName,GFun,Def}}|Defs],
    {Fun, Mps#mps{defs = Defs1}}.


get_guard(Type, #mps{defs = Defs}) ->
    {Type, {_Fun, _FN, GFun, _Def}} = proplists:lookup(Type, Defs),
    GFun.
    

gen_var(QRec) ->
    Vn = erl_syntax:variable_name(QRec),
    SVn = atom_to_list(Vn),
    SVn1 = SVn ++ "1",
    erl_syntax:revert(erl_syntax:variable(SVn1)).


%%
%% Formats error messages for compiler 
%%
format_error({unexpected_type_encode, Type}) ->
    format("Don't know how to encode type ~p", [Type]);
format_error({unexpected_type_decode, Type}) ->
    format("Don't know how to decode type ~p", [Type]).

format(Format, Args) ->
    io_lib:format(Format, Args).
