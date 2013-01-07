-module(jsonrec_encode).

-include_lib("meta/include/meta_syntax.hrl").
-include("parsers.hrl").


-export([encode_gen/4, encode_gen_encoder/4]).

-export([integer_to_json/1, float_to_json/1,
         binary_to_json/1, string_to_json/1,
         boolean_to_json/1, atom_to_json/1]).

-export([format_error/1]).


-define(TYPE_METHODS_OPT, type_methods).
-define(TYPE_SURROGATES_OPT, type_surrogates).
-define(NAME_HANDLER_OPT, name_handler).
-define(ENCODE_ATTR, encode).

-define(re(Syntax), erl_syntax:revert(Syntax)).

-define(TYPE(Type, Args),
        {type, _Ln3, Type, Args}).
-define(ATOM_TYPE(Atom),
        {atom, _Ln, Atom}).
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
         Type}).
-define(TYPED_FIELD(Name, Type, Default),
        {typed_record_field,
         ?FIELD(Name, Default),
         Type}).

-define(RECORD_QUOTE(Name),
        {record, _Ln, Name, _Args}).
-define(TYPE_QUOTE(Name, Args),
        {call, _Ln1, {atom, _Ln2, Name}, Args}).
-define(LIST_QUOTE(Elem),
        {cons, _Ln1, Elem, {nil, _Ln2}}).


-record(mps,
        {defs = [],
         subs = [],
         attrs = [],
         name_conv,
         types = gb_sets:new()}).

-record(def_funs,
        {fun_name,
         v_fun,
         g_fun,
         def}).

encode_gen(QArg, Type, Info, Options) ->
    code_gen(fun fetch/3, QArg, Type, Info, Options).

encode_gen_encoder(QArg, Type, Info, Options) ->
    code_gen(fun gen_encode/3, QArg, Type, Info, Options).

code_gen(CodeFun, QArg, Type, Info, Options) ->
    Type1 = norm_type_quote(?e(Type)),
    Subs = proplists:get_value(?TYPE_METHODS_OPT, ?e(Options), []),
    Subs1 = [norm_type(T) || T <- Subs],                      
    NameFun = proplists:get_value(?NAME_HANDLER_OPT, ?e(Options),
                                  fun atom_to_list/1),
    Attrs = meta:reify_attributes(?ENCODE_ATTR, ?e(Info)),
    Mps = #mps{
      defs = [],
      subs = Subs1,
      attrs = Attrs,
      name_conv = NameFun},
    {VFun, Mps1} = CodeFun(type_ref(Type1), ?e(Info), Mps),
    RDefs = lists:reverse(Mps1#mps.defs),
    Fs = [?q(?s(FN) = ?s(Def))
          || {_, #def_funs{fun_name = FN, def = Def}} <- RDefs,
             Def /= none],
    Call = VFun(QArg),
    ?v(?re(erl_syntax:block_expr(
             ?s(parsers:sequence(Fs))
             ++ [?s(Call)]))).


%% %%
%% %% Encoding
%% %%
gen_encode({record, [{atom, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    {Def, Mps1} = encode_fields(Fields, Info, Mps),
    QTag = ?v(?re(erl_parse:abstract(RecName))),
    GFun = fun(Item) ->
                   ?q(is_record(?s(Item), ?s(QTag)))
           end,
    add_fun_def(Type, Def, Mps1, id, GFun);
gen_encode({list, [InnerType]}, Info, Mps) ->
    encode_list(InnerType, Info, Mps);

gen_encode({union, Types} = Type, Info, Mps) ->
    {Def, Mps1} = encode_union(Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_encode({integer, []} = Type, _Info, Mps) ->
    P = ?q(?MODULE:integer_to_json),
    encode_basic_pred(Type, P, ?q(is_integer), Mps);
gen_encode({binary, []} = Type, _Info, Mps) ->
    P = ?q(?MODULE:binary_to_json),
    encode_basic_pred(Type, P, ?q(is_binary), Mps);
gen_encode({string, []} = Type, _Info, Mps) ->
    P = ?q(?MODULE:string_to_json),
    encode_basic_pred(Type, P, ?q(is_list), Mps);
gen_encode({float, []} = Type, _Info, Mps) ->
    P = ?q(?MODULE:float_to_json),
    encode_basic_pred(Type, P, ?q(is_float), Mps);
gen_encode({boolean, []} = Type, _Info, Mps) ->
    P = ?q(?MODULE:boolean_to_json),
    encode_basic_pred(Type, P, ?q(is_boolean), Mps);
gen_encode({atom, []} = Type, _Info, Mps) ->
    P = ?q(?MODULE:atom_to_json),
    encode_basic_pred(Type, P, ?q(is_atom), Mps);

gen_encode({atom, Atom} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           if
                               Atom =:= undefined ->
                                   ?q(undefined);
                               true ->
                                   ?v(?re(erl_parse:abstract(
                                            <<$\",(atom_to_binary(Atom, utf8))/binary, $\">>)))
                           end
                   end
           end,
    GFun = fun(Item) ->
                   ?q(?s(Item) =:= ?s(?v(?re(erl_parse:abstract(Atom)))))
           end,
    add_fun_def(Type, none, Mps, VFun, GFun);

gen_encode({any, []} = Type, _Info, Mps) ->
    FunVFun = fun(_) ->
                   fun(Item) ->
                           Item
                   end
           end,
    GFun = fun(Item) ->
                   ?q(is_binary(?s(Item)))
           end,
    add_fun_def(Type, none, Mps, FunVFun, GFun);

gen_encode({_UserType, _Args} = Type, Info, Mps) ->
    encode_underlying(Type, Info, Mps);
            
gen_encode(Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type, Type).

encode_fields(Fields, Info, Mps) ->
    NFs = lists:zip(lists:seq(2, length(Fields)+1), Fields),
    {Es,Mps1} = lists:mapfoldl(
                fun({I,T}, M) ->
                        encode_field(I, T, Info, M)
                end, Mps, NFs),
    Es1 = lists:reverse(Es),
    {?q(fun(_Rec) ->
                [<<"{">>, ?s(loop(?r(_Rec), ?q([]), Es1)), <<"}">>]
        end),
     Mps1}.

loop(QRec, QAcc, [F]) ->
    ?q(case ?s(F(QRec, QAcc)) of
           [] ->
               [];
           [_|Es] -> Es
       end);
loop(QRec, QAcc, [F|Fs]) ->
    ?q(begin
           Acc = ?s(F(QRec, QAcc)),
           ?s(loop(QRec, ?r(Acc), Fs))
       end).

    

encode_field(Ind, ?FIELD(Fn), Info, Mps) ->
    encode_record(Ind, Fn, {any, []}, Info, Mps);
encode_field(Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    encode_record(Ind, Fn, {any, []}, Info, Mps);
encode_field(Ind, ?TYPED_FIELD(Fn, Type), Info, Mps) ->
    encode_record(Ind, Fn, type_ref(Type), Info, Mps);
encode_field(Ind, ?TYPED_FIELD(Fn, Type, _Def), Info, Mps) ->
    encode_record(Ind, Fn, type_ref(Type), Info, Mps).

encode_record(Ind, Fn, Type, Info, #mps{name_conv = NC} = Mps) ->
    {VFun, Mps1} = fetch(Type, Info, Mps),
    AFN = ?v(?re(erl_parse:abstract("\"" ++ NC(Fn) ++ "\""))),
    QInd = ?v(?re(erl_parse:abstract(Ind))),
    DefFun = fun(QRec, QAcc) ->
                     case nullable_kind(Type) of
                         nullable ->
                             ?q(begin
                                    V = ?s(VFun(?q(element(?s(QInd), ?s(QRec))))),
                                    if V =/= undefined ->
                                            [<<$,>>, ?s(AFN), <<":">>, V
                                             | ?s(QAcc)];
                                       true ->
                                            ?s(QAcc)
                                    end
                                end);
                         non_nullable ->
                             ?q([<<$,>>, ?s(AFN), <<":">>,
                                 ?s(VFun(?q(element(?s(QInd), ?s(QRec)))))
                                 | ?s(QAcc)]);
                         null ->
                             QAcc
                     end
             end,
    {DefFun, Mps1}.


%%
%% General decode/encode functions
%%
fetch(Type, Info, Mps) ->
    case proplists:lookup(Type, Mps#mps.subs) of
        none ->
            Attrs = Mps#mps.attrs,
            case proplists:lookup(Type, Attrs) of
                none ->
                    case proplists:lookup(Type, Mps#mps.defs) of
                        {Type, #def_funs{v_fun = Fun}} ->
                            {Fun, Mps};
                        none ->
                            Ts = Mps#mps.types,
                            case gb_sets:is_member(Type, Ts) of
                                false ->
                                    Ts1 = gb_sets:add(Type, Ts),
                                    gen_encode(Type, Info, Mps#mps{types = Ts1});
                                true ->
                                    meta:error(?MODULE, loop, Type)
                            end
                    end;
                {Type, {_,Args} = SType} when is_list(Args) ->
                    fetch(SType, Info, Mps);
                {Type, Fun} ->
                    FunVFun = json_fun(Fun),
                    add_fun_def(Type, none, Mps, FunVFun)
            end;
        {Type, {_,Args} = SType} when is_list(Args) ->
            fetch(SType, Info, Mps);
        {Type, Fun} ->
            FunVFun = json_fun(Fun),
            add_fun_def(Type, none, Mps, FunVFun)
    end.

encode_list(InnerType, Info, Mps) -> 
    {Fun, Mps1} = fetch(type_ref(InnerType), Info, Mps),
    Def = ?q(fun([]) ->
                     <<"[]">>;
                (Es) ->
                     [<<$[>> |
                      tl(lists:foldl(fun(E, Acc) ->
                                             [<<$,>>, ?s(Fun(?r(E))) | Acc]
                                     end,
                                     [<<$]>>], lists:reverse(Es)))]
             end),
    add_fun_def({list, [InnerType]}, Def, Mps1).

encode_union(Types, Info, Mps) ->
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
    Def = ?v(?re(erl_syntax:fun_expr(Cs))),
    {Def, MpsN}.

encode_underlying({_, Args} = Type, Info, Mps) ->
    Type1 =  meta:reify_type(Type, Info),
    {_, Type2, []} = ground_type(Type1, Args),
    TR = type_ref(Type2),
    {VFun, Mps1} = fetch(TR, Info, Mps),
    FunVFun = fun(_) -> VFun end,
    GFun = get_guard(TR, Mps1),
    add_fun_def(Type, none, Mps1, FunVFun, GFun).

encode_basic_pred(Type, FunName, QGuardFun, Mps) ->
    FunVFun = fun(_) ->
                   fun(Item) ->
                           ?q(?s(FunName)(?s(Item)))
                   end
           end,
    GFun = fun(Item) ->
                   ?q(?s(QGuardFun)(?s(Item)))
           end,
    add_fun_def(Type, none, Mps, FunVFun, GFun).


%%
%% JSON emitter functions 
%%
integer_to_json(I) ->
    list_to_binary(integer_to_list(I)).

float_to_json(F) ->
    list_to_binary(io_lib_format:fwrite_g(F)).

binary_to_json(B) ->    
    [$", escape_binary(B), $"].

string_to_json(Str) ->
    binary_to_json(list_to_binary(Str)).

boolean_to_json(true) ->
    <<"true">>;
boolean_to_json(false) ->
    <<"false">>.

atom_to_json(A) ->
    <<$\", (atom_to_binary(A, utf8))/binary, $\">>.


escape_binary(Bin) ->
    escape_iter(Bin, Bin, 0, []).

escape_iter(<<>>, Bin, _, []) ->
    Bin;
escape_iter(<<>>, Bin, _, Acc) ->
    lists:reverse([Bin|Acc]);
escape_iter(<<$", Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Rest, 0, [<<"\\\"">>, binary:part(Bin, 0, Len) | Acc]);
escape_iter(<<$\\, Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Rest, 0, [<<"\\\\">>, binary:part(Bin, 0, Len) | Acc]);
escape_iter(<<$/, Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Rest, 0, [<<"\\/">>, binary:part(Bin, 0, Len) | Acc]);
escape_iter(<<$\b, Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Rest, 0, [<<"\\b">>, binary:part(Bin, 0, Len) | Acc]);
escape_iter(<<$\f, Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Rest, 0, [<<"\\f">>, binary:part(Bin, 0, Len) | Acc]);
escape_iter(<<$\n, Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Rest, 0, [<<"\\n">>, binary:part(Bin, 0, Len) | Acc]);
escape_iter(<<$\r, Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Rest, 0, [<<"\\r">>, binary:part(Bin, 0, Len) | Acc]);
escape_iter(<<$\t, Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Rest, 0, [<<"\\t">>, binary:part(Bin, 0, Len) | Acc]);

escape_iter(<<_/utf8, Rest/binary>>, Bin, Len, Acc) ->
    escape_iter(Rest, Bin, Len+1, Acc);
escape_iter(Inv, _, _, _) ->
    error({invalid_unicode, Inv}).


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
    {list, [InnerType]}.

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
    add_fun_def(Type, Def, Mps, id).

add_fun_def(Type, Def, Mps, FunVFun) ->
    add_fun_def(Type, Def, Mps, FunVFun, none).

add_fun_def(Type, Def, Mps, id, GFun) ->
    FunVFun = fun(QFunName) ->
                   fun(Item) ->
                           ?q(?s(QFunName)(?s(Item)))
                   end
              end,
    add_fun_def(Type, Def, Mps, FunVFun, GFun);
add_fun_def(Type, Def, #mps{defs = Defs} = Mps, FunVFun, GFun) ->
    Ind = length(Defs),
    FN = list_to_atom("Fun" ++ integer_to_list(Ind)),
    FunName = ?v(?re(erl_syntax:revert(erl_syntax:variable(FN)))),
    VFun = FunVFun(FunName),
    FDef = #def_funs
        {fun_name = FunName,
         v_fun = VFun,
         def = Def,
         g_fun = GFun},
    Defs1 = [{Type,FDef}|Defs],
    {VFun, Mps#mps{defs = Defs1}}.


get_guard(Type, #mps{defs = Defs}) ->
    #def_funs{g_fun = GFun} = proplists:get_value(Type, Defs),
    GFun.


json_fun({Mod,Fun}) ->
    QM = ?v(?re(erl_parse:abstract(Mod))),
    QF = ?v(?re(erl_parse:abstract(Fun))),
    fun(_) ->
            fun(Item) ->
                    ?q(?s(QM):?s(QF)(?s(Item)))
            end
    end;
json_fun(LocalFun) ->
    QFun = ?v(?re(erl_parse:abstract(LocalFun))),
    fun(_) ->
            fun(Item) ->
                    ?q(?s(QFun)(?s(Item)))
            end
    end.

nullable_kind({atom, undefined}) ->
     null;
nullable_kind(Type) ->
    case is_undefinable(Type) of
        true ->
            nullable;
        false ->
            non_nullable
    end.

is_undefinable({union, Types}) ->
    lists:any(fun is_undefinable/1, Types);
is_undefinable({atom, undefined}) ->
    true;
is_undefinable(_Type) ->
    false.


%%
%% Formats error messages for compiler 
%%
format_error({unexpected_type, Type}) ->
    format("Don't know how to encode type ~p", [Type]);
format_error({loop, Type}) ->
    format("Cannot handle recursive type definition for type ~p", [Type]).

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).


%%
%% Utils
%%

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
