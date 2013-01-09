%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2013, Eduard Sergeev
%%% @doc
%%% JSON "decode" code generator
%%%
%%% This module uses `parsers' monadic parser combinator library
%%% and a set of JSON parsers from `json_parsers'
%%% @end
%%% Created : 28 Nov 2012 by <eduard.sergeev@gmail.com>
%%%-------------------------------------------------------------------
-module(jsonrec_decode).

-include_lib("meta/include/meta_syntax.hrl").
-include("parsers.hrl").
-include("json_parsers.hrl").


-export([decode_gen/4, decode_gen_parser/4]).

-export([format_error/1]).


-define(PARSERS_OPT, parsers).
-define(SURROGATES_OPT, surrogate_types).
-define(NAME_HANDLER_OPT, name_handler).
-define(DECODE_ATTR, decode).

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
        {defs = dict:new(),
         subs :: dict(),
         name_conv :: fun((atom()) -> string()),
         types = gb_sets:new()}).

-record(def_funs,
        {parser :: parsers:q_parser(any())}).


-type decode_options() :: [decode_option()].
-type decode_option() :: parsers() | name_handler().

-type parsers() :: {parsers, [parser_type_ref() | parser_fun_ref()]}.
%% This option is used to specify custom parser for a type.

-type parser_type_ref() :: {record, Name :: atom()} |
                           {TypeName :: atom(), Args :: [parser_type_ref()]}.

-type parser_fun_ref() :: local_fun_ref() | remote_fun_ref().
%% A reference to parser {@type parsers:parser()} function

-type local_fun_ref() :: FunName :: atom().
-type remote_fun_ref() :: {Mod :: atom(), FunName :: atom()}.

-type name_handler() :: {name_handler, name_conv()}.
%% Forces decoder to preprocess all record field names
%% using specified function {@type name_conv()}

-type name_conv() :: fun((atom()) -> string()).


%%--------------------------------------------------------------------
%% @doc
%% Generates JSON "decode" function body for JSON de-serialization
%% using type annotation.
%%
%% Note: This is `meta' function and normally it should not be used directly.
%% Use `?decode_gen/2' and `?decode_gen/3' macros instead.
%% <ul>
%% <li>`Arg' is {@type meta:quote()} reference to generated function input parameter</li>
%% <li>`Type' is {@type meta:quote()} of the type being JSON decoded</li>
%% <li>`Info' is {@type meta:info()} structure returned by {@link meta:reify/0}</li>
%% <li>`Options' is {@type meta:quote()} of a list of options {@type decode_options()}</li>
%% </ul>
%%
%% Typical usage:
%% ```decode(Bin) ->
%%      ?decode_gen(#my_record{}, Bin).'''
%% Upon compilation this code will generate parsing code which populates `#my_record{}'
%% record if parsing succeeds.
%% @end
%%--------------------------------------------------------------------
-spec decode_gen(Arg, Type, Info, Options) -> meta:quote(FunBody) when
      Arg :: meta:quote(Var :: any()),
      Type :: meta:quote(Type :: any()),
      Info :: meta:quote(meta:info()),
      Options :: meta:quote(decode_options()),
      FunBody :: any().
decode_gen(Arg, Type, Info, Options) ->
    Parser = code_gen(fun fetch/3, Type, Info, Options),
    ?q(case ?s(to_parser(Parser, Arg)) of
           {ok, {Val, _}} ->
               {ok, Val};
           {error, _} = E ->
               E
       end).

%%--------------------------------------------------------------------
%% @doc
%% Generates JSON parser using type annotation.
%%
%% Similar to {@link decode_gen/3} but produces the code which is
%% {@type parsers:parser()}
%% so it can be used in place where parser is expected
%% (like in {@type parser_fun_ref()} option)
%%
%% This is `meta' function and normally it should not be used directly.
%% Use `?decode_gen_parser/2' and `?decode_gen_parser/3' macros instead.
%% <ul>
%% <li>`Arg' is {@type meta:quote()} reference to generated function input parameter</li>
%% <li>`Type' is {@type meta:quote()} of the type being JSON decoded</li>
%% <li>`Info' is {@type meta:info()} structure returned by {@link meta:reify/0}</li>
%% <li>`Options' is {@type meta:quote()} of a list of options {@type decode_options()}</li>
%% </ul>
%% @end
%%--------------------------------------------------------------------
-spec decode_gen_parser(Arg, Type, Info, Options) -> meta:quote(FunBody) when
      Arg :: meta:quote(Var :: any()),
      Type :: meta:quote(Type :: any()),
      Info :: meta:quote(meta:info()),
      Options :: meta:quote(decode_options()),
      FunBody :: any().
decode_gen_parser(QBin, Type, Info, Options) ->
    Parser = code_gen(fun gen_decode/3, Type, Info, Options),
    to_parser(Parser, QBin).


code_gen(CodeFun, Type, Info, Options) ->
    Type1 = norm_type_quote(?e(Type)),
    Attrs = meta:reify_attributes(?DECODE_ATTR, ?e(Info)),
    AEs = lists:flatten(proplists:get_all_values(?PARSERS_OPT, Attrs)),
    ASs = lists:flatten(proplists:get_all_values(?SURROGATES_OPT, Attrs)),
    OEs = proplists:get_value(?PARSERS_OPT, ?e(Options), []),
    OSs = proplists:get_value(?SURROGATES_OPT, ?e(Options), []),
    Subs = [handle_parser(E) || E <- AEs ++ OEs]
        ++ [handle_surrogate(S) || S <- ASs ++ OSs],
    DSubs = dict:from_list(Subs),
    NameFun = proplists:get_value(?NAME_HANDLER_OPT, ?e(Options),
                                  fun erlang:atom_to_list/1),
    Mps = #mps{
      subs = DSubs,
      name_conv = NameFun},
    {Parser, _} = CodeFun(type_ref(Type1), ?e(Info), Mps),
    right(lift(?q(json_parsers:ws_p)), Parser).


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
    add_fun_def(Type, Parser1, Mps1);    

gen_decode({list, [InnerType]}, Info, Mps) ->
    code_list(InnerType, Info, Mps);

gen_decode({union, Types} = Type, Info, Mps) ->
    {Parser, Mps1} = code_union(Types, Info, Mps),
    add_fun_def(Type, Parser, Mps1);

gen_decode({integer, []} = Type, _Info, Mps) ->
    P = ?q(json_parsers:integer_p),
    code_basic(Type, P, Mps);

gen_decode({binary, []} = Type, _Info, Mps) ->
    P = ?q(json_parsers:string_p),
    code_basic(Type, P, Mps);
gen_decode({string, []} = Type, _Info, Mps) ->
    P = bind(
          string(),
          fun(S) ->
                  return(
                    ?q(binary_to_list(?s(S))))
          end),
    add_fun_def(Type, P, Mps);
gen_decode({float, []} = Type, _Info, Mps) ->
    P = ?q(json_parsers:float_p),
    code_basic(Type, P, Mps);
gen_decode({boolean, []} = Type, _Info, Mps) ->
    P = ?q(json_parsers:boolean_p),
    code_basic(Type, P, Mps);
gen_decode({atom, []} = Type, _Info, Mps) ->
    Parser =
        bind(
          string(),
          fun(S) ->
                  return(
                    ?q(binary_to_existing_atom(?s(S), utf8)))
          end),
    add_fun_def(Type, Parser, Mps);

gen_decode({atom, undefined} = Type, _Info, Mps) ->
    P = ?q(json_parsers:null_p),
    code_basic(Type, P, Mps);

gen_decode({atom, Atom} = Type, _Info, Mps) ->
    Parser =
        right(
          match(
            ?v(?re(erl_parse:abstract(
                     "\"" ++ atom_to_list(Atom) ++ "\"")))),
          return(?v(?re(erl_parse:abstract(Atom))))),
    add_fun_def(Type, Parser, Mps);

gen_decode({any, []} = Type, _Info, Mps) ->
    P = ?q(json_parsers:any_json_p),
    code_basic(Type, P, Mps);

gen_decode({tuple, _Args} = Type, _Info, _Mps) ->
    meta:error(?MODULE, tuple_unsupported, Type);

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
    P = object(Es),
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
    {Parser, Mps1} = fetch(Type, Info, Mps),
    QFn = ?v(?re(erl_parse:abstract(NC(Fn)))),
    QInd = ?v(?re(erl_parse:abstract(Index))),
    Triple = {QFn, Parser, QInd},
    {Triple, Mps1}. 


%%
%% General decode/encode functions
%%
fetch(Type, Info, Mps) ->
    case dict:find(Type, Mps#mps.subs) of
        error ->
            case dict:find(Type, Mps#mps.defs) of
                {ok, #def_funs{parser = Parser}} ->
                    {Parser, Mps};
                error ->
                    Ts = Mps#mps.types,
                    case gb_sets:is_member(Type, Ts) of
                        false ->
                            Ts1 = gb_sets:add(Type, Ts),
                            gen_decode(Type, Info, Mps#mps{types = Ts1});
                        true ->
                            meta:error(?MODULE, loop, Type)
                    end
            end;
        {ok, {_,Args} = SType} when is_list(Args) ->
            fetch(SType, Info, Mps);
        {ok, Fun} ->
            QFun = json_fun(Fun),
            add_fun_def(Type, lift(QFun), Mps)
    end.

code_list(InnerType, Info, Mps) -> 
    {Parser, Mps1} = fetch(type_ref(InnerType), Info, Mps),
    add_fun_def({list, [InnerType]}, array(Parser), Mps1).

code_union(Types, Info, Mps) ->
    P0 = fail(?q(none_matches)),
    {Parser, MpsN} =
        lists:foldl(
          fun(TA, {P1, Mps1}) ->
                  Type = type_ref(TA),
                  {P2, Mps2} = fetch(Type, Info, Mps1),
                  P3 = mplus(P2, P1),
                  {P3, Mps2}
          end, {P0, Mps}, Types),
    {Parser, MpsN}.

code_underlying({_, Args} = Type, Info, Mps) ->
    Type1 =  meta:reify_type(Type, Info),
    {_, Type2, []} = ground_type(Type1, Args),
    TR = type_ref(Type2),
    %% fetch(TR, Info, Mps).
    {Parser, Mps1} = fetch(TR, Info, Mps),
    add_fun_def(Type, Parser, Mps1).



code_basic(Type, ParserFun, Mps) ->
    add_fun_def(Type, lift(ParserFun), Mps).


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


add_fun_def(Type, Parser, #mps{defs = Defs} = Mps) ->
    FDef = #def_funs{parser = Parser},
    Defs1 = dict:store(Type, FDef, Defs),
    {Parser, Mps#mps{defs = Defs1}}.


json_fun({Mod,Fun}) ->
    QM = ?v(?re(erl_parse:abstract(Mod))),
    QF = ?v(?re(erl_parse:abstract(Fun))),
    ?q(?s(QM):?s(QF));
json_fun(LocalFun) ->
    ?v(?re(erl_parse:abstract(LocalFun))).


handle_parser({Type, FunRef}) ->
    Type1 = option_type_norm(Type),
    FunRef1 = option_fun_norm(FunRef), 
    {Type1, FunRef1};
handle_parser(Inv) ->
    meta:error(?MODULE, {invalid_parser_option, Inv}).

handle_surrogate({Type, Surrogate}) ->
    {option_type_norm(Type), option_type_norm(Surrogate)};
handle_surrogate(Inv) ->
    meta:error(?MODULE, {invalid_surrogate_option, Inv}).

option_type_norm({record, Name}) when is_atom(Name) ->
    {record,[{atom,Name}]};
option_type_norm({Type, Args})
  when is_atom(Type) andalso is_list(Args) ->
    Args1 = [option_type_norm(A) || A <- Args],
    {Type, Args1};
option_type_norm(Inv) ->
    meta:error(?MODULE, {invalid_type_reference, Inv}).

option_fun_norm({Mod, FunName} = FunRef)
  when is_atom(Mod) andalso is_atom(FunName) ->
    FunRef;
option_fun_norm(FunName) when is_atom(FunName) ->
    FunName;
option_fun_norm(Inv) ->
    meta:error(?MODULE, {invalid_fun_reference, Inv}).


%%
%% Formats error messages for compiler 
%%
-spec format_error(any()) -> iolist().
format_error({unexpected_type_decode, Type}) ->
    format("Don't know how to decode type ~p",
           [type_to_list(Type)]);
format_error({loop, Type}) ->
    format("Cannot handle recursive type definition for type ~p",
           [type_to_list(Type)]);
format_error({invalid_parser_option, Inv}) ->
    format("Invalid 'parser' option: ~p", [Inv]);
format_error({invalid_surrogate_option, Inv}) ->
    format("Invalid 'surrogate_type' option: ~p", [Inv]);
format_error({invalid_type_reference, Inv}) ->
    format("Invalid type reference in option list: ~p", [Inv]);
format_error({invalid_fun_reference, Inv}) ->
    format("Invalid function reference in option list: ~p", [Inv]);
format_error({tuple_unsupported, Type}) ->
    format("Don't know how to decode tuple type ~p",
           [type_to_list(Type)]).

type_to_list({union, Types}) ->
    string:join([type_to_list(T) || T <- Types], " | ");
type_to_list({record, [{atom, Name}]}) ->
    lists:flatten(format("#~s{}", [Name]));
type_to_list({atom, Atom}) ->
    atom_to_list(Atom);
type_to_list({Name, Args}) ->
    Args1 = string:join([type_to_list(T) || T <- Args], ","),
    lists:flatten(format("~s(~s)", [Name, Args1])).

format(Format, Args) ->
    io_lib:format(Format, Args).

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
