-module(jsonrec_encode).

-include_lib("meta/include/meta_syntax.hrl").
-include("parsers.hrl").


-export([encode_gen/4]).

-export([integer_to_binary/1]).

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

-record(def_funs,
        {fun_name,
         v_fun,
         g_fun,
         def}).

encode_gen(QRec, Type, Info, Options) ->
    encode_gen(encode, QRec, Type, Info, Options).

encode_gen(Attr, QArg, Type, Info, Options) ->
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
      code_fun = fun gen_encode/3,
      name_conv = NameFun},
    {VFun, Mps1} = gen_encode(type_ref(Type1), ?e(Info), Mps),
    RDefs = lists:reverse(Mps1#mps.defs),
    Fs = [?q(?s(FN) = ?s(Def))
          || {_, #def_funs{fun_name = FN, def = Def}} <- RDefs,
             Def /= none],
    %% io:format("~p~n", [?e(lists:nth(2, Fs))]),
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
    add_fun_def(Type, Def, Mps1);
gen_encode({list, [InnerType]}, Info, Mps) ->
    encode_list(InnerType, Info, Mps);

gen_encode({union, Types} = Type, Info, Mps) ->
    {Def, Mps1} = encode_union(Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_encode({integer, []} = Type, _Info, Mps) ->
    P = ?q(?MODULE:integer_to_binary),
    encode_basic_pred(Type, P, ?q(is_integer), Mps);

%% gen_encode({binary, []} = Type, _Info, Mps) ->
%%     P = ?q(?MODULE:integer_to_binary),
%%     code_basic_pred(Type, ?q(is_binary), Mps);

%% gen_encode({float, []} = Type, _Info, Mps) ->
%%     code_basic_pred(Type, ?q(is_float), Mps);
%% gen_encode({boolean, []} = Type, _Info, Mps) ->
%%     code_basic_pred(Type, ?q(is_boolean), Mps);
%% gen_encode({atom, []} = Type, _Info, Mps) ->
%%     VFun = fun(_) ->
%%                    fun(Item) ->
%%                            ?q(atom_to_binary(?s(Item), utf8))
%%                    end
%%            end,
%%     GFun = fun(Item) ->
%%                    ?q(is_atom(?s(Item)))
%%            end,
%%     add_fun_def(Type, none, Mps, GFun, VFun);

gen_encode({atom, Atom} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           if
                               Atom =:= undefined ->
                                   ?q(undefined);
                               true ->
                                   ?v(?re(erl_parse:abstract(
                                            atom_to_binary(Atom, utf8))))
                           end
                   end
           end,
    GFun = fun(Item) ->
                   ?q(?s(Item) =:= ?s(?v(?re(erl_parse:abstract(Atom)))))
           end,
    add_fun_def(Type, none, Mps, VFun, GFun);

%% gen_encode({any, []} = Type, _Info, Mps) ->
%%     GFun = fun(_Item) ->
%%                    ?q(true)
%%            end,
%%     code_basic(Type, GFun, Mps);

%% gen_encode({_UserType, _Args} = Type, Info, Mps) ->
%%     code_underlying(Type, Info, Mps);
            
gen_encode(Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type_encode, Type).

encode_fields(Fields, Info, Mps) ->
    NFs = lists:zip(lists:seq(2, length(Fields)+1), Fields),
    {Es,Mps1} = lists:mapfoldl(
                fun({I,T}, M) ->
                        encode_field(I, T, Info, M)
                end, Mps, NFs),
    %% Cons = fun(QReq) ->
    %%                fun(F, {QAcc, QE}) ->
    %%                        ?q(Acc = ?s(H(QRec, QAcc))
    %%                        ?q(?s(H(QReq))(?s(T)))
    %%                end
    %%                           end,
    {?q(fun(Record) ->
                [<<"{">>, ?s(loop(?r(Record), ?q([]), Es)), <<"}">>]
        end),
     Mps1}.

loop(QRec, QAcc, [F]) ->
    F(QRec, QAcc);
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
    AFN = ?v(?re(erl_parse:abstract(NC(Fn)))),
    QInd = ?v(?re(erl_parse:abstract(Ind))),
    DefFun = fun(QRec, QAcc) ->
                     ?q(if
                            element(?s(QInd), ?s(QRec))  =/= undefined ->
                                [?s(AFN), <<":">>, ?s(VFun(?q(element(?s(QInd), ?s(QRec)))))
                                 | ?s(QAcc)];
                            true ->
                                ?s(QAcc)
                        end)
             end,
    {DefFun, Mps1}.


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
                        {Type, #def_funs{fun_name = Fun}} ->
                            {Fun, Mps};
                        none ->
                            CodeFun(Type, Info, Mps)
                    end;
                {Type, {_,Args} = SType} when is_list(Args) ->
                    fetch(SType, Info, Mps);
                {Type, Fun} ->
                    QFun = json_fun(Fun),
                    add_fun_def(Type, none, Mps, QFun)
            end;
        {Type, {_,Args} = SType} when is_list(Args) ->
            fetch(SType, Info, Mps);
        {Type, Fun} ->
            QFun = json_fun(Fun),
            add_fun_def(Type, none, Mps, QFun)
    end.

encode_list(InnerType, Info, Mps) -> 
    {Fun, Mps1} = fetch(type_ref(InnerType), Info, Mps),
    Def = ?q(fun(Es) ->
                     lists:map(fun(E) -> ?s(Fun(?r(E))) end, Xs)
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


%% code_underlying({_, Args} = Type, Info, Mps) ->
%%     Type1 =  meta:reify_type(Type, Info),
%%     {_, Type2, []} = ground_type(Type1, Args),
%%     TR = type_ref(Type2),
%%     fetch(TR, Info, Mps).


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
%% Default data conversion functions
%%
atom_to_msbinary(Atom) ->
    list_to_binary(atom_to_mslist(Atom)).

atom_to_mslist(Atom) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    Parts = string:tokens(List, "_"),
    Capitalized = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Parts),
    lists:concat(Capitalized). 

integer_to_binary(I) ->
    list_to_binary(integer_to_list(I)).

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

%% ground_type({Name, Def, Params}, Args) ->
%%     PAs = lists:zip(Params, Args),
%%     Ls = lists:map(
%%            fun({{var, _, P}, TA}) ->
%%                    {P, TA}
%%            end, PAs),

%%     DC = dict:from_list(Ls),
%%     Fun = fun({var, _Ln, P}) ->
%%                   dict:fetch(P, DC);
%%              (Smt) ->
%%                   Smt
%%           end,
%%     Def1 = map(Fun, Def),
%%     {Name, Def1, []}.


add_fun_def(Type, Def, Mps) ->
    FunVFun = fun(QFunName) ->
                   fun(Item) ->
                           ?q(?s(QFunName)(?s(Item)))
                   end
           end,
    add_fun_def(Type, Def, Mps, FunVFun).

add_fun_def(Type, Def, Mps, FunVFun) ->
    add_fun_def(Type, Def, Mps, FunVFun, none).
    

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

%% get_pattern(Type, #mps{defs = Defs}) ->
%%     #def_funs{pattern = PFun} = proplists:get_value(Type, Defs),
%%     PFun.
   
%% gen_var(QRec) ->
%%     Vn = erl_syntax:variable_name(QRec),
%%     SVn = atom_to_list(Vn),
%%     SVn1 = SVn ++ "1",
%%     erl_syntax:revert(erl_syntax:variable(SVn1)).


json_fun({Mod,Fun}) ->
    QM = ?v(?re(erl_parse:abstract(Mod))),
    QF = ?v(?re(erl_parse:abstract(Fun))),
    ?q(?s(QM):?s(QF));
json_fun(LocalFun) ->
    ?v(?re(erl_parse:abstract(LocalFun))).


%%
%% Formats error messages for compiler 
%%
format_error({unexpected_type_encode, Type}) ->
    format("Don't know how to encode type ~p", [Type]).

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).


%%
%% Utils
%%


%% encode(Rec) ->
%%     Acc = [],
%%     Acc1 = if
%%                Rec#rec.id =/= undefined ->
%%                    [Rec#rec.id|Acc];
%%                true ->
%%                    Acc
%%            end,
%%     Acc2 = if
%%                Rec#rec.f1 =/= undefined ->
%%                    [Rec#rec.f1|Acc1];
%%                true ->
%%                    Acc1
%%            end,
%%     [<<${>>, Acc2, <<$}>>].
