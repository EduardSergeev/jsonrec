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

-include_lib("meta/include/meta.hrl").
-include_lib("meta/include/meta_syntax.hrl").
-include("parsers.hrl").
-include("json_parsers.hrl").
-include("jsonrec_code.hrl").


-export([decode_gen/4, decode_gen_parser/4]).

-export([format_error/1]).


-define(PARSER_OPT, parser).
-define(SURROGATE_OPT, surrogate_type).
-define(NAME_HANDLER_OPT, name_handler).
-define(JSONREC_ATTR, jsonrec).

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


-record(def_funs,
        {parser :: parsers:q_parser(any())}).


-type decode_options() :: [decode_option()].
-type decode_option() :: name_handler() | parser() | surrogate().

-type name_handler() :: {?NAME_HANDLER_OPT, name_handler_def()}.
%% Forces decoder_gen to preprocess record field names using specified function
-type name_handler_def() :: jsonrec_code:name_handler_def() | [name_handler_def()].

-type parser() :: {?PARSER_OPT, parser_def()}.
%% This option is used to specify custom parser for a type or field.
-type parser_def() :: jsonrec_code:coder_handler_def() | [parser_def()].

-type surrogate() :: {?SURROGATE_OPT, surrogate_def()}.
%% This options is used to treat a type or a field as if it was of another type
-type surrogate_def() :: jsonrec_code:surrogate_def() | [surrogate_def()].


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


code_gen(CodeFun, Type, QInfo, QOptions) ->
    Info = ?e(QInfo),
    Options = ?e(QOptions),
    Type1 = norm_type_quote(?e(Type)),
    Attrs = lists:flatten(meta:reify_attributes(?JSONREC_ATTR, Info)),
    AEs = flatget_all_values(?PARSER_OPT, Attrs),
    ASs = flatget_all_values(?SURROGATE_OPT, Attrs),
    OEs = flatget_all_values(?PARSER_OPT, Options),
    OSs = flatget_all_values(?SURROGATE_OPT, Options),
    Subs = [jsonrec_code:handle_surrogate(S) || S <- ASs ++ OSs]
        ++ [jsonrec_code:handle_coder(E) || E <- AEs ++ OEs],
    DSubs = dict:from_list(lists:flatten(Subs)),

    ANCs = flatget_all_values(?NAME_HANDLER_OPT, Attrs),
    ONCs = flatget_all_values(?NAME_HANDLER_OPT, Options),
    NConvs = [{default, fun atom_to_list/1}]
        ++ lists:flatmap(
             fun(NC) ->
                     jsonrec_code:handle_nameconv(NC, Info)
             end, ANCs ++ ONCs),
    DNConvs = dict:from_list(NConvs),

    Mps = #mps{
      subs = DSubs,
      n_convs = DNConvs},
    {Parser, _} = CodeFun(type_ref(Type1), Info, Mps),
    right(lift(?q(json_parsers:ws_p)), Parser).


gen_decode({record, [{atom, RecName}]} = Type, Info, Mps) ->
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    {Parser, Mps1} = gen_object_parser(RecName, Fields, Info, Mps),
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



gen_object_parser(RecName, Types, Info, Mps) ->
    NTs = lists:zip(lists:seq(2, length(Types)+1), Types),
    {Es, Mps1} = lists:mapfoldl(
                  fun({N,T},M) ->
                          decode_field(RecName, N, T, Info, M)
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


decode_field(RecName, Ind, ?FIELD(Fn), Info, Mps) ->
    decode_record(RecName, Ind, Fn, {any, []}, Info, Mps);
decode_field(RecName, Ind, ?FIELD(Fn, _Def), Info, Mps) ->
    decode_record(RecName, Ind, Fn, {any, []}, Info, Mps);
decode_field(RecName, Ind, ?TYPED_FIELD(Fn, Type), Info, Mps) ->
    decode_record(RecName, Ind, Fn, type_ref(Type), Info, Mps);
decode_field(RecName, Ind, ?TYPED_FIELD(Fn, Type, _Def), Info, Mps) ->
    decode_record(RecName, Ind, Fn, type_ref(Type), Info, Mps).

decode_record(RecName, Ind, FieldName, Type, Info, Mps) ->
    NC = jsonrec_code:fetch_nameconv(RecName, FieldName, Mps),
    {Parser, Mps1} = fetch({RecName, FieldName, Type}, Info, Mps),
    QFn = ?v(?re(erl_parse:abstract(NC(FieldName)))),
    QInd = ?v(?re(erl_parse:abstract(Ind))),
    Triple = {QFn, Parser, QInd},
    {Triple, Mps1}. 


%%
%% General decode/encode functions
%%
fetch({RecName, FieldName, Type}, Info, Mps) ->
    fetch(Type, {{record,RecName}, FieldName}, Info, Mps);
fetch(Type, Info, Mps) ->
    fetch(Type, Type, Info, Mps).

fetch(Type, SubKey, Info, Mps) ->
    case fetch_sub(SubKey, Type, Mps) of
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

fetch_sub(Type, Type, Mps) ->
    dict:find(Type, Mps#mps.subs);
fetch_sub(SubKey, Type, Mps) ->
    case dict:find(SubKey, Mps#mps.subs) of
        error ->
            fetch_sub(Type, Type, Mps);
        Val ->
            Val
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
flatget_all_values(Key, PropList) ->
    lists:flatten(proplists:get_all_values(Key, PropList)).

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
