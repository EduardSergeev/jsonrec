-module(jsonrec_decode).

-include_lib("meta/include/meta_syntax.hrl").
-include("parsers.hrl").


-export([decode_gen/4]).

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
        {%%value,
         fun_name,
         %% guard,
         %% pattern,
         def
}).

%% encode_gen(QRec, Type, Info, Options) ->
%%     code_gen(fun gen_encode/4, encode, QRec, Type, Info, Options).


decode_gen(QStr, Type, Info, Options) ->
    code_gen(fun gen_decode/3, decode, QStr, Type, Info, Options).


code_gen(CodeFun, Attr, QArg, Type, Info, Options) ->
    Type1 = norm_type_quote(?e(Type)),
    Subs = proplists:get_value(?TYPE_METHODS_OPT, ?e(Options), []),
    Subs1 = [norm_type(T) || T <- Subs],                      
    NameFun = proplists:get_value(?NAME_HANDLER_OPT, ?e(Options),
                                  fun atom_to_mslist/1),
    Attrs = meta:reify_attributes(Attr, ?e(Info)),
    Mps = #mps{
      defs = [],
      subs = Subs1,
      attrs = Attrs,
      code_fun = CodeFun,
      name_conv = NameFun},
    {FunName, Mps1} = CodeFun(type_ref(Type1), ?e(Info), Mps),
    RDefs = lists:reverse(Mps1#mps.defs),
    Fs = [?q(?s(FN) = ?s(Def))
          || {_, #def_funs{fun_name = FN, def = Def}} <- RDefs,
             Def /= none],
    Call = ?q(case ?s(FunName)(?s(QArg), 0) of
                  {ok, {Val, _}} ->
                      {ok, Val};
                  {error, _} = E ->
                      E
              end),
    ?v(?re(erl_syntax:block_expr(
             ?s(parsers:sequence(Fs))
             ++ [?s(Call)]))).

gen_decode({record, [{atom, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    {Parser, Mps1} = gen_object_parser(Fields, Info, Mps),
    Size = ?v(?re(erl_parse:abstract(length(Fields) + 1))),
    Parser1 = bind(
               Parser,
               fun(QFs) ->
                       return(
                         ?q(erlang:make_tuple(
                              ?s(Size),
                              undefined,
                              ?s(with_defaults(RecName, Fields, QFs)))))
               end),
    Def = ?q(fun(Inp, Pos) ->
                     ?s(inst_body(
                          Parser1,
                          ?r(Inp), ?r(Pos)))
             end),
    add_fun_def(Type, Def, Mps1);    

gen_decode({list, [InnerType]}, Info, Mps) ->
    code_list(InnerType, Info, Mps);

gen_decode({union, Types} = Type, Info, Mps) ->
    {Def,Mps1} = code_union(Types, Info, Mps),
    add_fun_def(Type, Def, Mps1);

gen_decode({integer, []} = Type, _Info, Mps) ->
    P = ?q(parsers:integer_p),
    code_basic(Type, P, Mps);

gen_decode({binary, []} = Type, _Info, Mps) ->
    P = ?q(parsers:string_p),
    code_basic(Type, P, Mps);

gen_decode({float, []} = Type, _Info, Mps) ->
    P = ?q(parsers:float_p),
    code_basic(Type, P, Mps);
gen_decode({boolean, []} = Type, _Info, Mps) ->
    P = ?q(parsers:boolean_p),
    code_basic(Type, P, Mps);
gen_decode({atom, []} = Type, _Info, Mps) ->
    Def = ?q(fun(Inp, Pos) ->
                     ?s(inst_body(
                          bind(
                            parsers:string(),
                            fun(S) ->
                                    return(
                                      ?q(binary_to_existing_atom(?s(S), utf8)))
                            end),
                          ?r(Inp), ?r(Pos)))
             end),    
    add_fun_def(Type, Def, Mps);

gen_decode({atom, undefined} = Type, _Info, Mps) ->
    P = ?q(parsers:null_p),
    code_basic(Type, P, Mps);

gen_decode({atom, Atom} = Type, _Info, Mps) ->
    Def = ?q(fun(Inp, Pos) ->
                     ?s(inst_body(
                          right(
                            match(
                              ?v(?re(erl_parse:abstract(
                                       "\"" ++ atom_to_list(Atom) ++ "\"")))),
                            return(?v(?re(erl_parse:abstract(Atom))))),
                          ?r(Inp), ?r(Pos)))
             end),
    add_fun_def(Type, Def, Mps);

%% gen_decode({any, []} = Type, _Info, Mps) ->
%%     GFun = fun(_Item) ->
%%                    ?q(true)
%%            end,
%%     code_basic(Type, GFun, Mps);

gen_decode({_UserType,_Args} = Type, Info, Mps) ->
    code_underlying(Type, Info, Mps);
    
gen_decode(Type, _Info, _Mps) ->
    meta:error(?MODULE, unexpected_type_decode, Type).



gen_object_parser(Types, Info, Mps) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    {Es, Mps1} = lists:mapfoldl(
                  fun({N,T},M) ->
                          decode_field(N, T, Info, M)
                  end, Mps, NTs),
    P = parsers:object(Es),
    {P, Mps1}.

with_defaults(RecName, Types, Tail) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    Ds = [decode_default(N, ?v(QDef))
          || {N, ?TYPED_FIELD(_, _, QDef)} <- NTs],
    QName = ?v(?re(erl_parse:abstract(RecName))),
    Tag = ?q({1,?s(QName)}),
    ?v(?re(erl_syntax:list([?s(Tag)|?s(parsers:sequence(Ds))], ?s(Tail)))).

decode_default(Ind, QDef) ->
    QInd = ?v(?re(erl_parse:abstract(Ind))),
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
    QFn = ?v(?re(erl_parse:abstract(NC(Fn)))),
    QInd = ?v(?re(erl_parse:abstract(Index))),
    Triple = {QFn, parsers:lift(Fun), QInd},
    %% io:format("Triple: ~p~n", [?e(QInd)]),
    {Triple, Mps1}. 


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

code_list(InnerType, Info, Mps) -> 
    {Fun, Mps1} = fetch(type_ref(InnerType), Info, Mps),
    Def = ?q(fun(Inp, Pos) ->
                     ?s(inst_body(parsers:array(lift(Fun)),
                                  ?r(Inp), ?r(Pos)))
             end),
    add_fun_def({list, [InnerType]}, Def, Mps1).

code_union(Types, Info, Mps) ->
    P0 = fail(?q(none_matches)),
    {PN, MpsN} =
        lists:foldl(
          fun(TA, {P1, Mps1}) ->
                  Type = type_ref(TA),
                  {Fun, Mps2} = fetch(Type, Info, Mps1),
                  P2 = mplus(lift(Fun), P1),
                  {P2, Mps2}
          end, {P0, Mps}, Types),
    Def = ?q(fun(Inp, Pos) ->
                     ?s(inst_body(PN, ?r(Inp), ?r(Pos)))
             end),
    {Def, MpsN}.

code_underlying({_, Args} = Type, Info, Mps) ->
    Type1 =  meta:reify_type(Type, Info),
    {_, Type2, []} = ground_type(Type1, Args),
    TR = type_ref(Type2),
    fetch(TR, Info, Mps).

code_basic(Type, Parser, Mps) ->
    add_fun_def(Type, none, Mps, Parser).

%%
%% Default data conversion functions
%%
%% atom_to_msbinary(Atom) ->
%%     list_to_binary(atom_to_mslist(Atom)).

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


add_fun_def(Type, Def, #mps{defs = Defs} = Mps) ->
    Ind = length(Defs),
    FunName = list_to_atom("Fun" ++ integer_to_list(Ind)),
    QVFunName = ?v(?re(erl_syntax:revert(erl_syntax:variable(FunName)))),

    add_fun_def(Type, Def, Mps, QVFunName).

add_fun_def(Type, Def, #mps{defs = Defs} = Mps, FunName) ->
    FDef = #def_funs
        {fun_name = FunName,
         def = Def},
    Defs1 = [{Type,FDef}|Defs],
    {FunName, Mps#mps{defs = Defs1}}.

json_fun({Mod,Fun}) ->
    QM = ?v(?re(erl_parse:abstract(Mod))),
    QF = ?v(?re(erl_parse:abstract(Fun))),
    ?q(?s(QM):?s(QF));
json_fun(LocalFun) ->
    ?v(?re(erl_parse:abstract(LocalFun))).


%%
%% Formats error messages for compiler 
%%
format_error({unexpected_type_decode, Type}) ->
    format("Don't know how to decode type ~p", [Type]).

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
