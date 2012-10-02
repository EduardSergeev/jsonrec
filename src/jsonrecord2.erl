-module(jsonrecord2).

-include_lib("meta/include/meta.hrl").
-include_lib("meta/include/meta_syntax.hrl").

-export([encode_gen/3, encode_gen/4,
         decode_gen/3, decode_gen/4]).

-export([format_error/1]).

-define(TYPE_METHODS_OPT, type_methods).
-define(TYPE_SURROGATES_OPT, type_surrogates).
-define(NAME_HANDLER_OPT, name_handler).

-define(re(Syntax), erl_syntax:revert(Syntax)).


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

-define(RECORD_QUOTE(Name),
        {record, _Ln, Name, _Args}).
-define(TYPE_QUOTE(Name, Args),
        {call, _Ln1, {atom, _Ln2, Name}, Args}).
-define(LIST_QUOTE(Elem),
        {cons, _Ln1, Elem, {lin, _Ln2}}).

-record(mps,
        {defs = [],
         subs = [],
         attrs = [],
         code_fun,
         name_conv}).

encode_gen(QRec, Type, Info) ->
    encode_gen(QRec, Type, Info, []).

encode_gen(QRec, Type, Info, Options) ->
    code_gen(fun gen_encode/3, encode, QRec, Type, Info, Options).


decode_gen(QStr, Type, Info) ->
    decode_gen(QStr, Type, Info, []).

decode_gen(QStr, Type, Info, Options) ->
    code_gen(fun gen_decode/3, decode, QStr, Type, Info, Options).


code_gen(CodeFun, Attr, QArg, Type, Info, Options) ->
    Type1 = norm_type_quote(?e(Type)),
    Subs = proplists:get_value(?TYPE_METHODS_OPT, ?e(Options), []),
    Subs1 = [norm_type(T) || T <- Subs],                      
    NameFun = proplists:get_value(?NAME_HANDLER_OPT, ?e(Options),
                                  fun atom_to_msbinary/1),
    Attrs = meta:reify_attributes(Attr, ?e(Info)),
    Mps = #mps{
      defs = [],
      subs = Subs1,
      attrs = Attrs,
      code_fun = CodeFun,
      name_conv = NameFun},
    {Fun, Mps1} = CodeFun(type_ref(Type1), ?e(Info), Mps),
    Fs = [?q(?s(FN) = ?s(Def))
          || {_,{_,FN,_GFun,Def}} <- lists:reverse(Mps1#mps.defs),
             Def /= none],
    Re = ?q(?v(?re(erl_syntax:block_expr(?s(sequence(Fs))
                                         ++ [?s(Fun(QArg))])))),
    %% io:format("Re: ~p~n", [Re]),
    Re.


%%
%% Encoding
%%
gen_encode({record, [{atom, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    {Def, Mps1} = encode_fields(Fields, Info, Mps),
    GFun = fun(Item) ->
                   ?q(is_record(?s(Item),?s(?q(?v(?re(erl_parse:abstract(RecName)))))))
           end,
    add_fun_def(Type, Def, Mps1, GFun);
gen_encode({list, [InnerType]}, Info, Mps) ->
    code_list(InnerType, Info, Mps);

gen_encode({union, Types} = Type, Info, Mps) ->
    {Def,Mps1} = code_union(Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_encode({integer, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_integer), Mps);
gen_encode({binary, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_binary), Mps);
gen_encode({float, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_float), Mps);
gen_encode({boolean, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_boolean), Mps);
gen_encode({atom, []} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           ?q(atom_to_binary(?s(Item), utf8))
                   end
           end,
    GFun = fun(Item) ->
                   ?q(is_atom(?s(Item)))
           end,
    add_fun_def(Type, none, Mps, GFun, VFun);

gen_encode({atom, Atom} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           if
                               Atom =:= undefined ->
                                   ?q(undefined);
                               true ->
                                   ?q(?v(?re(erl_parse:abstract(
                                               atom_to_binary(Atom, utf8)))))
                           end
                   end
           end,
    GFun = fun(Item) ->
                   ?q(?s(Item) =:= ?s(?q(?v(?re(erl_parse:abstract(Atom))))))
           end,
    add_fun_def(Type, none, Mps, GFun, VFun);

gen_encode({any, []} = Type, _Info, Mps) ->
    GFun = fun(_Item) ->
                   ?q(true)
           end,
    code_basic(Type, GFun, Mps);

gen_encode({_UserType, _Args} = Type, Info, Mps) ->
    code_underlying(Type, Info, Mps);
            
gen_encode(Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type_encode, Type).

encode_fields(Fields, Info, Mps) ->
    NFs = lists:zip(lists:seq(2, length(Fields)+1), Fields),
    {Es,Mps1} = lists:mapfoldl(
                fun({I,T}, M) ->
                        encode_field(I, T, Info, M)
                end, Mps, NFs),
    Cons = fun(QReq) ->
                   fun(H,T) ->
                           ?q(?s(H(QReq))(?s(T)))
                   end
           end,
    {?q(fun(Record) ->
                {struct,
                 ?s(lists:foldr(Cons(?r(Record)),
                                ?q([]), Es))}
        end),
     Mps1}.

encode_field(Ind, ?FIELD(Fn), Info, Mps) ->
    encode_record(Ind, Fn, {any, []}, Info, Mps);
encode_field(Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    encode_record(Ind, Fn, {any, []}, Info, Mps);
encode_field(Ind, ?TYPED_FIELD(Fn, Type), Info, Mps) ->
    encode_record(Ind, Fn, type_ref(Type), Info, Mps);
encode_field(Ind, ?TYPED_FIELD(Fn, Type, _Def), Info, Mps) ->
    encode_record(Ind, Fn, type_ref(Type), Info, Mps).

encode_record(Ind, Fn, Type, Info, #mps{name_conv = NC} = Mps) ->
    {Fun, Mps1} = fetch(Type, Info, Mps),
    AFN = ?q(?v(?re(erl_parse:abstract(NC(Fn))))),
    QInd = ?q(?v(?re(erl_parse:abstract(Ind)))),
    DefFun = fun(QRec) ->
                     ?q(fun(Acc) ->
                                V = element(?s(QInd), ?s(QRec)),
                                if 
                                    V =:= undefined ->
                                        Acc;
                                    true ->
                                        [{?s(AFN), ?s(Fun(?r(V)))} | Acc]
                                end
                        end)
             end,
    {DefFun, Mps1}.


%%
%% Decoding
%%
gen_decode({record, [{atom, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    {FTI, Mps1} = gen_field_to_integer(Fields, Info, Mps),
    Size = ?q(?v(?re(erl_parse:abstract(length(Fields) + 1)))),
    %% QFs = gen_var(QStr),
    Def = ?q(fun({struct, Struct}) ->
                     Fs1 = [?s(FTI)(F,V) || {F,V} <- Struct],
                     Fs2 = [T || T <- Fs1, is_tuple(T)],
                     erlang:make_tuple(
                       ?s(Size),
                       undefined,
                       ?s(with_defaults(RecName, Fields, ?r(Fs2))))
             end),
    add_fun_def(Type, Def, Mps1);    

gen_decode({list, [InnerType]}, Info, Mps) ->
    code_list(InnerType, Info, Mps);

gen_decode({union, Types} = Type, Info, Mps) ->
    {Def,Mps1} = code_union(Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_decode({integer, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_integer), Mps);
gen_decode({binary, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_binary), Mps);
gen_decode({float, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_float), Mps);
gen_decode({boolean, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_boolean), Mps);
gen_decode({atom, []} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           ?q(binary_to_existing_atom(?s(Item), utf8))
                   end
           end,
    GFun = fun(Item) ->
                   ?q(is_binary(?s(Item)))
           end,
    add_fun_def(Type, none, Mps, GFun, VFun);

gen_decode({atom, Atom} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           ?q(?v(?re(erl_parse:abstract(Atom))))
                   end
           end,
    Bin = atom_to_binary(Atom, utf8),
    GFun = fun(Item) ->
                   if
                       Atom =:= undefined ->
                           ?q(?s(Item) =:= undefined);
                       true ->
                           ?q(?s(Item) =:= ?s(?q(?v(?re(erl_parse:abstract(Bin))))))
                   end
           end,
    add_fun_def(Type, none, Mps, GFun, VFun);


gen_decode({any, []} = Type, _Info, Mps) ->
    GFun = fun(_Item) ->
                   ?q(true)
           end,
    code_basic(Type, GFun, Mps);

gen_decode({_UserType,_Args} = Type, Info, Mps) ->
    code_underlying(Type, Info, Mps);
    
gen_decode(Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type_decode, Type).



gen_field_to_integer(Types, Info, Mps) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    {Es,Mps1} = lists:mapfoldl(
                  fun({N,T},M) ->
                          decode_field(N, T, Info, M)
                  end, Mps, NTs),
    QLast = ?q(?v(hd(erl_syntax:fun_expr_clauses(
                       ?s(?q(fun(_,_) -> undefined end)))))),
    Es1 = Es ++ [QLast],
    Ast = ?q(?v(?re(erl_syntax:fun_expr(?s(sequence(Es1)))))),
    {Ast, Mps1}.

with_defaults(RecName, Types, Tail) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    Ds = [decode_default(N, ?q(?v(QDef)))
          || {N, ?TYPED_FIELD(_, _, QDef)} <- NTs],
    QName = ?q(?v(?re(erl_parse:abstract(RecName)))),
    Tag = ?q({1,?s(QName)}),
    ?q(?v(?re(erl_syntax:list([?s(Tag)|?s(sequence(Ds))], ?s(Tail))))).

decode_default(Ind, QDef) ->
    QInd = ?q(?v(?re(erl_parse:abstract(Ind)))),
    ?q({?s(QInd), ?s(QDef)}).


decode_field(Ind, ?FIELD(Fn), Info, Mps) ->
    decode_record(Ind, Fn, {any, []}, Info, Mps);
decode_field(Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    decode_record(Ind, Fn, {any, []}, Info, Mps);
decode_field(Ind, ?TYPED_FIELD(Fn, Type), Info, Mps) ->
    decode_record(Ind, Fn, type_ref(Type), Info, Mps);
decode_field(Ind, ?TYPED_FIELD(Fn, Type, _Def), Info, Mps) ->
    decode_record(Ind, Fn, type_ref(Type), Info, Mps).

decode_record(Index, Fn, Type, Info, #mps{name_conv = NC} = Mps) ->
    {Fun, Mps1} = fetch(Type, Info, Mps),
    QFn = ?q(?v(?re(erl_parse:abstract(NC(Fn))))),
    QInd = ?q(?v(?re(erl_parse:abstract(Index)))),
    Def = ?q(?v(?re(hd(erl_syntax:fun_expr_clauses(
                         ?s(?q(fun(?s(QFn), V) ->
                                       {?s(QInd), ?s(Fun(?r(V)))}
                               end))))))),
    {Def, Mps1}. 


%%
%% General decode/encode functions
%%
fetch(Type, Info, #mps{code_fun = CodeFun} = Mps) ->
    case proplists:lookup(Type, Mps#mps.subs) of
        none ->
            Attrs = Mps#mps.attrs,
            case proplists:lookup(Type, Attrs) of
                none ->
                    case proplists:lookup(Type, Mps#mps.defs) of
                        {Type, {Fun, _FN, _GFun, _Def}} ->
                            {Fun, Mps};
                        none ->
                            CodeFun(Type, Info, Mps)
                    end;
                {Type, {_,Args} = SType} when is_list(Args) ->
                    fetch(SType, Info, Mps);
                {Type, Fun} ->
                    VFun = json_fun(Fun),
                    add_fun_def(Type, none, Mps, none, VFun)
            end;
        {Type, {_,Args} = SType} when is_list(Args) ->
            fetch(SType, Info, Mps);
        {Type, Fun} ->
            VFun = json_fun(Fun),
            add_fun_def(Type, none, Mps, none, VFun)
    end.

code_list(InnerType, Info, Mps) -> 
    {Fun, Mps1} = fetch(type_ref(InnerType), Info, Mps),
    Def = ?q(fun(Xs) ->
                     %% [?s(Fun(?q(X))) || X <- Xs]
                     lists:map(fun(X) -> ?s(Fun(?r(X))) end, Xs)
             end),
    add_fun_def({list, [InnerType]}, Def, Mps1).

code_union(Types, Info, Mps) ->
    {Cs, MpsN} =
        lists:mapfoldl(
          fun(TA, Mps1) ->
                  Type = type_ref(TA),
                  {VFun, Mps2} = fetch(Type, Info, Mps1),
                  QF = case get_guard(Type, Mps2) of
                           none ->
                               ?q(fun(V) ->
                                          ?s(VFun(?r(V)))
                                  end);
                           GFun ->
                               ?q(fun(V) when ?s(GFun(?r(V))) ->
                                          ?s(VFun(?r(V)))
                                  end)
                       end,
                  [C] = erl_syntax:fun_expr_clauses(?e(QF)),
                  {C,Mps2}
          end, Mps, Types),
    Def = ?q(?v(?re(erl_syntax:fun_expr(Cs)))),
    {Def, MpsN}.

code_underlying({_, Args} = Type, Info, Mps) ->
    Type1 =  meta:reify_type(Type, Info),
    {_, Type2, []} = ground_type(Type1, Args),
    TR = type_ref(Type2),
    {Fun, Mps1} = fetch(TR, Info, Mps),
    VFun = fun(_) -> Fun end,
    GFun = get_guard(TR, Mps1),
    add_fun_def(Type, none, Mps1, GFun, VFun).


code_basic(Type, GFun, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           Item
                   end
           end,
    add_fun_def(Type, none, Mps, GFun, VFun).

code_basic_pred(Type, QGuardFun, Mps) ->
    GFun = fun(Item) ->
                   ?q(?s(QGuardFun)(?s(Item)))
           end,
    VFun = fun(_) ->
                   fun(Item) ->
                           Item
                   end
           end,
    add_fun_def(Type, none, Mps, GFun, VFun).


%%
%% Default data conversion functions
%%
atom_to_msbinary(Atom) ->
    list_to_binary(atom_to_mslist(Atom)).

atom_to_mslist(Atom) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    Parts = string:tokens(List, "_"),
    Capitalized = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Parts),
    lists:concat(Capitalized). 

%%
%% Utils
%%
norm_type_quote(?RECORD_QUOTE(Name)) ->
    {record,[{atom,Name}]};
norm_type_quote(?TYPE_QUOTE(Name, Args)) ->
    Args1 = [norm_type_quote(A) || A <- Args],
    {Name, Args1};
norm_type_quote(?LIST_QUOTE(Elem)) ->
    InnerType = norm_type_quote(Elem),
    {list, InnerType}.


type_ref({type, _Ln, Tag, Args}) ->
    {Tag, [type_ref(A) || A <- Args]};
type_ref({atom, _Ln, Atom}) ->
    {atom, Atom};
type_ref(Converted) ->
    Converted.


norm_type({{record, Name}, _Def, []}) ->
    {record,[{atom,Name}]};
norm_type({Name, _Def, Args}) ->
    {Name, Args};
norm_type({_Name, _Args} = Type) ->
    Type.

ground_type({Name, Def, Params}, Args) ->
    PAs = lists:zip(Params, Args),
    Ls = lists:map(
           fun({{var, _, P}, TA}) ->
                   {P, TA}
           end, PAs),

    DC = dict:from_list(Ls),
    Fun = fun({var, _Ln, P}) ->
                  dict:fetch(P, DC);
             (Smt) ->
                  Smt
          end,
    Def1 = map(Fun, Def),
    {Name, Def1, []}.


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
    QVFunName = ?q(?v(?re(erl_syntax:variable(FunName)))),
    Fun = VFun(QVFunName),
    Defs1 = [{Type,{Fun,QVFunName,GFun,Def}}|Defs],
    {Fun, Mps#mps{defs = Defs1}}.


get_guard(Type, #mps{defs = Defs}) ->
    {Type, {_Fun, _FN, GFun, _Def}} = proplists:lookup(Type, Defs),
    GFun.
   

json_fun({Mod,Fun}) ->
    fun(_) ->
            fun(Item) -> 
                    QM = ?q(?v(?re(erl_parse:abstract(Mod)))),
                    QF = ?q(?v(?re(erl_parse:abstract(Fun)))),
                    ?q(?s(QM):?s(QF)(?s(Item)))
            end
    end;
json_fun(LocalFun) ->
    fun(_) ->
            fun(Item) -> 
                    QF = ?q(?v(?re(erl_parse:abstract(LocalFun)))),
                    ?q(?s(QF)(?s(Item)))
            end
    end.

%%
%% Formats error messages for compiler 
%%
format_error({unexpected_type_encode, Type}) ->
    format("Don't know how to encode type ~p", [Type]);
format_error({unexpected_type_decode, Type}) ->
    format("Don't know how to decode type ~p", [Type]).

format(Format, Args) ->
    io_lib:format(Format, Args).

%%
%% Depth-first map
%%
map(Fun, Form) when is_tuple(Form) ->
    Fs = tuple_to_list(Form),
    Fs1 = map(Fun, Fs),
    Form1 = list_to_tuple(Fs1),
    Fun(Form1);
map(Fun, Fs) when is_list(Fs) ->
    [map(Fun, F) || F <- Fs];
map(Fun, Smt) ->
    Fun(Smt).

sequence([]) ->
    ?q(?v([]));
sequence([Q|Qs]) ->
    ?q(?v([?s(Q)|?s(sequence(Qs))])).
