-module(jsonrec).

-include_lib("meta/include/meta.hrl").
-include_lib("meta/include/meta_syntax.hrl").

-export([encode_gen/4, decode_gen/4]).

-export([format_error/1]).

-export([integer/1, string/1,
         object/2]).

-define(TYPE_METHODS_OPT, type_methods).
-define(TYPE_SURROGATES_OPT, type_surrogates).
-define(NAME_HANDLER_OPT, name_handler).


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

-record(mps,
        {defs = [],
         subs = [],
         attrs = [],
         code_fun,
         name_conv}).

-record(def_funs,
        {value,
         fun_name,
         guard,
         pattern,
         def}).

%% -record(input,
%%         {bin,
%%          off,
%%          ln,
%%          col}).

encode_gen(QRec, Type, Info, Options) ->
    code_gen(fun gen_encode/4, encode, QRec, Type, Info, Options).


decode_gen(QStr, Type, Info, Options) ->
    code_gen(fun gen_decode/4, decode, QStr, Type, Info, Options).


code_gen(CodeFun, Attr, QArg, Type, Info, Options) ->
    Type1 = norm_type(Type),
    Subs = proplists:get_value(?TYPE_METHODS_OPT, Options, []),
    Subs1 = [norm_type(T) || T <- Subs],                      
    NameFun = proplists:get_value(?NAME_HANDLER_OPT, Options,
                                  fun atom_to_msbinary/1),
    Attrs = meta:reify_attributes(Attr, Info),
    Mps = #mps{
      defs = [],
      subs = Subs1,
      attrs = Attrs,
      code_fun = CodeFun,
      name_conv = NameFun},
    {Fun,Mps1} = CodeFun(QArg, type_ref(Type1), Info, Mps),
    RDefs = lists:reverse(Mps1#mps.defs),
    Fs = [?q(?s(FN) = ?s(Def))
          || {_,#def_funs{fun_name = FN, def = Def}} <- RDefs,
             Def /= none],
    erl_syntax:block_expr(Fs ++ [Fun(QArg)]).


%%
%% Encoding
%%
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
gen_encode(QRec, {list, [InnerType]}, Info, Mps) ->
    code_list(QRec, InnerType, Info, Mps);

gen_encode(QRec, {union, Types} = Type, Info, Mps) ->
    {Def,Mps1} = code_union(QRec, Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_encode(_, {integer, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_integer), Mps);
gen_encode(_, {binary, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_binary), Mps);
gen_encode(_, {float, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_float), Mps);
gen_encode(_, {boolean, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_boolean), Mps);
gen_encode(_, {atom, []} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           ?q(atom_to_binary(?s(Item), utf8))
                   end
           end,
    GFun = fun(Item) ->
                   ?q(is_atom(?s(Item)))
           end,
    add_fun_def(Type, none, Mps, GFun, VFun);

gen_encode(_, {atom, Atom} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           if
                               Atom =:= undefined ->
                                   ?q(undefined);
                               true ->
                                   erl_parse:abstract(
                                     atom_to_binary(Atom, utf8))
                           end
                   end
           end,
    GFun = fun(Item) ->
                   ?q(?s(Item) =:= ?s(erl_parse:abstract(Atom)))
           end,
    add_fun_def(Type, none, Mps, GFun, VFun);

gen_encode(_, {any, []} = Type, _Info, Mps) ->
    GFun = fun(_Item) ->
                   ?q(true)
           end,
    code_basic(Type, GFun, Mps);

gen_encode(QRec, {_UserType, _Args} = Type, Info, Mps) ->
    code_underlying(QRec, Type, Info, Mps);
            
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
    encode_record(QRec, Ind, Fn, {any, []}, Info, Mps);
encode_field(QRec, Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    encode_record(QRec, Ind, Fn, {any, []}, Info, Mps);
encode_field(QRec, Ind, ?TYPED_FIELD(Fn, Type), Info, Mps) ->
    encode_record(QRec, Ind, Fn, type_ref(Type), Info, Mps);
encode_field(QRec, Ind, ?TYPED_FIELD(Fn, Type, _Def), Info, Mps) ->
    encode_record(QRec, Ind, Fn, type_ref(Type), Info, Mps).

encode_record(QRec, Ind, Fn, Type, Info, #mps{name_conv = NC} = Mps) ->
    {Fun, Mps1} = fetch(QRec, Type, Info, Mps),
    AFN = erl_parse:abstract(NC(Fn)),
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
gen_decode(QStr, {record, [{atom, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    {FTI, Mps1} = gen_field_to_integer(QStr, Fields, Info, Mps),
    Size = erl_parse:abstract(length(Fields) + 1),
    QInp = gen_var(QStr),
    Def = ?q(fun(?s(QInp)) ->
                     {Fs, Inp2} = ?MODULE:object(?s(FTI), ?s(QInp)),
                     Fs1 = [T || T <- Fs, is_tuple(T)],
                     Res = erlang:make_tuple(
                             ?s(Size),
                             undefined,
                             ?s(with_defaults(RecName, Fields, ?q(Fs1)))),
                     {Res, Inp2}
             end),
    add_fun_def(Type, Def, Mps1);    

gen_decode(QStr, {list, [InnerType]}, Info, Mps) ->
    code_list(QStr, InnerType, Info, Mps);

gen_decode(QRec, {union, Types} = Type, Info, Mps) ->
    {Def,Mps1} = code_union(QRec, Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_decode(_, {integer, []} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           ?q(?MODULE:integer(?s(Item)))
                   end
           end,
    GFun = fun(_Item) ->
                   ?q((C =:= $-) orelse (C >= $0 andalso C =< $9))
           end,
    PFun = fun(Item) ->
                   ?q(<<C,_/binary>> = ?s(Item))
           end,
    add_fun_def(Type, none, Mps, GFun, VFun, PFun);

gen_decode(_, {binary, []} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           ?q(?MODULE:string(?s(Item)))
                   end
           end,
    PFun = fun(Item) ->
                   ?q(<<$",_/binary>> = ?s(Item))
           end,
    add_fun_def(Type, none, Mps, none, VFun, PFun);


gen_decode(_, {float, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_float), Mps);
gen_decode(_, {boolean, []} = Type, _Info, Mps) ->
    code_basic_pred(Type, ?q(is_boolean), Mps);
gen_decode(_, {atom, []} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(Item) ->
                           ?q(begin
                                  {Bin, Rest} = ?MODULE:string(?s(Item)),
                                  {binary_to_existing_atom(Bin, utf8), Rest}
                              end)
                   end
           end,
    PFun = fun(Item) ->
                   ?q(<<$",_/binary>> = ?s(Item))
           end,
    add_fun_def(Type, none, Mps, none, VFun, PFun);

gen_decode(_, {atom, undefined} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           ?q({undefined, Rest})
                   end
           end,
    PFun = fun(_Item) ->
                   ?q(<<"null", Rest/binary>>)
           end,
    add_fun_def(Type, none, Mps, none, VFun, PFun);

gen_decode(_, {atom, Atom} = Type, _Info, Mps) ->
    VFun = fun(_) ->
                   fun(_Item) ->
                           ?q({?s(erl_parse:abstract(Atom)), Rest})
                   end
           end,
    SAtom = format("\"~s\"", [Atom]),
    QSAtom = erl_parse:abstract(SAtom),
    PFun = fun(_Item) ->
                   ?q(<<?s(QSAtom), Rest/binary>>)
           end,

    add_fun_def(Type, none, Mps, none, VFun, PFun);


gen_decode(_, {any, []} = Type, _Info, Mps) ->
    GFun = fun(_Item) ->
                   ?q(true)
           end,
    code_basic(Type, GFun, Mps);

gen_decode(QStr, {_UserType,_Args} = Type, Info, Mps) ->
    code_underlying(QStr, Type, Info, Mps);
    
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

decode_record(QStr, Index, Fn, Type, Info, #mps{name_conv = NC} = Mps) ->
    {Fun, Mps1} = fetch(QStr, Type, Info, Mps),
    QFn = erl_parse:abstract(NC(Fn)),
    QInd = erl_parse:abstract(Index),
    [Def] = erl_syntax:fun_expr_clauses(
              ?q(fun(?s(QFn), V1) ->
                         {?s(QInd), ?s(Fun(?q(V1)))}
                 end)),
    {Def, Mps1}. 


%%
%% General decode/encode functions
%%
fetch(QPar, Type, Info, #mps{code_fun = CodeFun} = Mps) ->
    case proplists:lookup(Type, Mps#mps.subs) of
        none ->
            Attrs = Mps#mps.attrs,
            case proplists:lookup(Type, Attrs) of
                none ->
                    case proplists:lookup(Type, Mps#mps.defs) of
                        {Type, #def_funs{value = Fun}} ->
                            {Fun, Mps};
                        none ->
                            CodeFun(QPar, Type, Info, Mps)
                    end;
                {Type, {_,Args} = SType} when is_list(Args) ->
                    fetch(QPar, SType, Info, Mps);
                {Type, Fun} ->
                    VFun = json_fun(Fun),
                    add_fun_def(Type, none, Mps, none, VFun)
            end;
        {Type, {_,Args} = SType} when is_list(Args) ->
            fetch(QPar, SType, Info, Mps);
        {Type, Fun} ->
            VFun = json_fun(Fun),
            add_fun_def(Type, none, Mps, none, VFun)
    end.

code_list(QArg, InnerType, Info, Mps) -> 
    {Fun, Mps1} = fetch(QArg, type_ref(InnerType), Info, Mps),
    QXs = gen_var(QArg),
    Def = ?q(fun(?s(QXs)) ->
                     [?s(Fun(?q(X))) || X <- ?s(QXs)]
             end),
    add_fun_def({list, [InnerType]}, Def, Mps1).

code_union(QArg, Types, Info, Mps) ->
    {Cs, MpsN} =
        lists:mapfoldl(
          fun(TA, Mps1) ->
                  Type = type_ref(TA),
                  {VFun, Mps2} = fetch(QArg, Type, Info, Mps1),
                  PFun = get_pattern(Type, Mps2),
                  QV = ?q(V),
                  QF = case get_guard(Type, Mps2) of
                           none ->
                               ?q(fun(?s(PFun(QV))) ->
                                          ?s(VFun(QV))
                                  end);
                           GFun ->
                               ?q(fun(?s(PFun(QV))) when ?s(GFun(?q(V))) ->
                                          ?s(VFun(QV))
                                  end)
                       end,
                  [C] = erl_syntax:fun_expr_clauses(QF),
                  {C,Mps2}
          end, Mps, Types),
    Def = erl_syntax:revert(erl_syntax:fun_expr(Cs)),
    {Def, MpsN}.

code_underlying(QStr, {_, Args} = Type, Info, Mps) ->
    Type1 =  meta:reify_type(Type, Info),
    {_, Type2, []} = ground_type(Type1, Args),
    TR = type_ref(Type2),
    {Fun, Mps1} = fetch(QStr, TR, Info, Mps),
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
    Def1 = meta:map(Fun, Def),
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
    PFun = fun(Item) ->
                   Item
           end,
    add_fun_def(Type, Def, #mps{defs = Defs} = Mps, GFun, VFun, PFun).

add_fun_def(Type, Def, #mps{defs = Defs} = Mps, GFun, VFun, PFun) ->
    Ind = length(Defs),
    FunName = list_to_atom("Fun" ++ integer_to_list(Ind)),
    QVFunName = erl_syntax:revert(erl_syntax:variable(FunName)),
    Fun = VFun(QVFunName),
    FDef = #def_funs
        {value = Fun,
         fun_name = QVFunName,
         guard = GFun,
         pattern = PFun,
         def = Def},
    %% Defs1 = [{Type,{Fun, QVFunName, GFun, PFun, Def}}|Defs],
    Defs1 = [{Type,FDef}|Defs],
    {Fun, Mps#mps{defs = Defs1}}.


get_guard(Type, #mps{defs = Defs}) ->
    #def_funs{guard = GFun} = proplists:get_value(Type, Defs),
    GFun.

get_pattern(Type, #mps{defs = Defs}) ->
    #def_funs{pattern = PFun} = proplists:get_value(Type, Defs),
    PFun.
   
gen_var(QRec) ->
    Vn = erl_syntax:variable_name(QRec),
    SVn = atom_to_list(Vn),
    SVn1 = SVn ++ "1",
    erl_syntax:revert(erl_syntax:variable(SVn1)).


json_fun({Mod,Fun}) ->
    fun(_) ->
            fun(Item) -> 
                    QM = erl_parse:abstract(Mod),
                    QF = erl_parse:abstract(Fun),
                    ?q(?s(QM):?s(QF)(?s(Item)))
            end
    end;
json_fun(LocalFun) ->
    fun(_) ->
            fun(Item) -> 
                    QF = erl_parse:abstract(LocalFun),
                    ?q(?s(QF)(?s(Item)))
            end
    end.

%%
%% Parsing functions
%%
%% skip_ws(#input{bin = <<$\n,Rest/binary>>, off = O, ln = L} = Inp) ->
%%     skip_ws(Inp#input{bin = Rest, off = O + 1, ln = L + 1, col = 1});
%% skip_ws(#input{bin = <<W,Rest/binary>>, off = O, col = C} = Inp)
%%   when W =:= $\s orelse W =:= $\t orelse W =:= $\r ->
%%     skip_ws(Inp#input{bin = Rest, off = O + 1, col = C + 1});
%% skip_ws(Inp) ->
%%     Inp.
skip_ws(<<$\s, Rest/binary>>) ->
    skip_ws(Rest);
skip_ws(<<$\n, Rest/binary>>) ->
    skip_ws(Rest);
skip_ws(<<$\r, Rest/binary>>) ->
    skip_ws(Rest);
skip_ws(<<$\t, Rest/binary>>) ->
    skip_ws(Rest);
skip_ws(Rest) ->
    Rest.


object(ValueParser, Inp) ->
    case skip_ws(Inp) of
        <<"{", Inp1/binary>> ->
            fields_iter(ValueParser, Inp1, []);
        <<U, _/binary>> ->
            error({unexpected_input, <<U>>, {expected, <<${>>}});
        <<>> ->
            error(eof)
    end.
    
fields_iter(ValueParser, Inp, Rs) ->
    case pair(ValueParser, Inp) of
        {IV, Inp1} ->
            Rs1 = [IV | Rs],
            case skip_ws(Inp1) of
                <<",", Inp2/binary>> ->
                    fields_iter(ValueParser, Inp2, Rs1);
                <<"}", Rest/binary>> ->
                    {lists:reverse(Rs1), Rest};
                <<U, _/binary>> ->
                    error({unexpected_input, <<U>>, {expected, [<<$,>>,<<$}>>]}});
                <<>> ->
                    error(eof)
            end;
        undefined ->
            undefined
    end.

pair(ValueParser, Inp) ->    
    {Field, Inp1} = string(skip_ws(Inp)),
    case skip_ws(Inp1) of
        <<$:, Inp2/binary>> ->
            {Index, {Value, Rest}} = ValueParser(Field, skip_ws(Inp2)),
            true = is_binary(Rest),
            {{Index, Value}, Rest};
        <<U, _/binary>> ->
            error({unexpected_input, <<U>>, {expected, <<$:>>}});
        <<>> ->
            error(eof)
    end.

string(<<$", Inp1/binary>>) ->
    {Len, Rest} = string_iter(Inp1, 0),
    {binary:part(Inp1, 0, Len), Rest};
string(<<U, _/binary>>) ->
    error({unexpected_input, <<U>>, {expected, <<$">>}});
string(<<>>) ->
    error(eof).
            
string_iter(<<"\\\"", Rest/binary>>, Len) ->
    string_iter(Rest, Len + 2);
string_iter(<<$", Rest/binary>>, Len) ->
    {Len, Rest};
string_iter(<<_, Rest/binary>>, Len) ->
    string_iter(Rest, Len + 1);
string_iter(<<>>, _) ->
    error(eof).

integer(<<$-, Rest/binary>>) ->
    {I, Rest1} = integer_iter(Rest, 0),
    {-I, Rest1};
integer(<<D, Rest/binary>>) when (D >= $0) andalso (D =< $9) ->
    integer_iter(Rest, D - $0);
integer(<<U, _/binary>>) ->
    error({unexpected_input, <<U>>, {expected, [any_digit, <<"-">>]}});
integer(<<>>) ->
    error(eof).

integer_iter(<<D, Rest/binary>>, I) when (D >= $0) andalso (D =< $9) ->  
    integer_iter(Rest, I*10 + (D - $0));
integer_iter(Rest, I) ->
    {I, Rest}.




%%
%% Formats error messages for compiler 
%%
format_error({unexpected_type_encode, Type}) ->
    format("Don't know how to encode type ~p", [Type]);
format_error({unexpected_type_decode, Type}) ->
    format("Don't know how to decode type ~p", [Type]).

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
