%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2013, Eduard Sergeev
%%% @doc
%%% Common functionality of `jsonrec_encode' and `jsonrec_decode'
%%% @end
%%% Created : 28 Nov 2012 by <eduard.sergeev@gmail.com>
%%%-------------------------------------------------------------------
-module(jsonrec_code).

-include_lib("meta/include/meta.hrl").
-include_lib("meta/include/meta_syntax.hrl").
-include("parsers.hrl").
-include("jsonrec_code.hrl").

-export([handle_nameconv/2, fetch_nameconv/3,
        handle_coder/1, handle_surrogate/1,

        format_error/1]).

-export_type([name_handler_def/0,
              coder_handler_def/0,
              surrogate_def/0]).

%%-type name_handler() :: {name_handler, name_handler_def()}.
%% Forces decoder/encoder to preprocess all or some record field names
%% using specified function {@type name_conv()}

-type name_handler_def() :: name_handler_fun() | record_name_handler().
%% Forces encoder/parser to use modified record field names
%% preprocessed with specified function {@type name_handler_fun()}

-type record_name_handler() :: {extended_record_ref(), name_handler_fun()}.

-type name_handler_fun() :: fun_ref() | inline_fun().
-type fun_ref() :: local_fun_ref() | remote_fun_ref().
-type local_fun_ref() :: FunName :: atom().
-type remote_fun_ref() :: {Mod :: atom(), FunName :: atom()}.
-type inline_fun() :: fun((atom()) -> string()).

-type extended_record_ref() :: record_flat_ref() | record_with_fields_ref().
-type record_flat_ref() :: record_type_ref() | record_field_ref().
-type record_field_ref() :: {record_type_ref(), Field :: atom()}.
-type record_type_ref() :: {record, RecName :: atom()}.
-type record_with_fields_ref() :: {record_type_ref(), [Field :: atom()]}.
-type record_type() :: {record, [{atom,RecName :: atom()}]}.


-type coder_handler_def() :: {extended_type_ref(), code_fun_ref()}.
%% Forces encoder or parser to use specified finction {@type code_fun_ref()}
%% for encoding/parsing
%% (which supresses code_generation for {@type extended_type_ref()})

-type extended_type_ref() :: extended_record_ref() | grounded_type_ref().
-type flat_type_ref() :: record_flat_ref() | type_ref().
-type grounded_type_ref() :: record_type_ref() | type_ref().
-type type_ref() :: {TypeName :: atom(), TypeArgs :: [grounded_type_ref()]}.
-type grounded_type() :: record_type() | simple_type().
-type simple_type() :: {TypeName :: atom(), TypeArgs :: [grounded_type()]}. 

-type code_fun_ref() :: fun_ref().


-type surrogate_def() :: {extended_type_ref(), surrogate_type_ref()}.
%% Forces encoder/parser to generate encoding/parsing code 
%% for type {@type extended_type_ref()} as if it was type {@type extended_type_ref()}

-type surrogate_type_ref() :: grounded_type_ref().


-spec handle_nameconv(name_handler_def(), meta:info()) -> [{Key, Fun}] when
      Key :: record_flat_ref() | default,
      Fun ::  name_handler_fun().
handle_nameconv({ExtRecRef, ConvRef}, Info)
  when is_tuple(ExtRecRef) ->
    RecRefs = extended_record_ref_norm(ExtRecRef),
    Conv = handle_conv(ConvRef, Info),
    [ {RecRef, Conv} || RecRef <- RecRefs ];
handle_nameconv(ConvRef, Info) ->
    [{default, handle_conv(ConvRef, Info)}].

handle_conv(LocFun, Info)
  when is_atom(LocFun) ->
    fun(Arg) ->
            meta:local_apply(LocFun, [Arg], Info)
    end;
handle_conv({Mod, Fun}, _Info)
  when is_atom(Mod) andalso is_atom(Fun) ->
    fun Mod:Fun/1;
handle_conv(Fun, _Info) when is_function(Fun, 1) ->
    Fun;
handle_conv(Inv, _Info) ->
    meta:error(?MODULE, {invalid_name_fun_ref, Inv}).


-spec fetch_nameconv(atom(), atom(), #mps{}) -> name_handler_fun().
fetch_nameconv(RecName, FieldName, #mps{n_convs = NCs}) ->
    case dict:find({{record,RecName}, FieldName}, NCs) of
        error ->
            case dict:find({record,RecName}, NCs) of
                error ->
                    dict:fetch(default, NCs);
                {ok, Fun} ->
                    Fun
            end;
        {ok, Fun} ->
            Fun
    end.


-spec handle_coder(coder_handler_def()) -> [{ForType, UseFun}] when
      ForType :: flat_type_ref(),
      UseFun :: fun_ref().
handle_coder({ExtTypeRef, FunRef}) ->
    TypeRefs = extended_type_ref_norm(ExtTypeRef),
    FunDef = code_fun_norm(FunRef),
    [ case TypeRef of
          {{record, _}, _} ->
              {TypeRef, FunDef};
          Grounded ->
              {grounded_to_typedef(Grounded), FunDef}
      end || TypeRef <- TypeRefs ];
handle_coder(Inv) ->
    meta:error(?MODULE, {invalid_coder_option, Inv}).


-spec handle_surrogate(surrogate_def()) -> [{InsteadOf, UseType}] when
      InsteadOf :: flat_type_ref(),
      UseType ::  grounded_type().
handle_surrogate({ExtTypeRef, SurrogateRef}) ->
    TypeRefs = extended_type_ref_norm(ExtTypeRef),
    SurrogateDef = grounded_to_typedef(SurrogateRef),
    [ case TypeRef of
          {{record, _}, _} ->
              {TypeRef, SurrogateDef};
          Grounded ->
              {grounded_to_typedef(Grounded), SurrogateDef}
      end || TypeRef <- TypeRefs ];
handle_surrogate(Inv) ->
    meta:error(?MODULE, {invalid_surrogate_option, Inv}).


%%
%% Spec checking and normalisation function
%%

extended_type_ref_norm({{record, _}, _} = RecRef) ->
    extended_record_ref_norm(RecRef);
extended_type_ref_norm({record, _} = RecRef) ->
    extended_record_ref_norm(RecRef);
extended_type_ref_norm(TypeRef) ->
    [grounded_type_ref_norm(TypeRef)].

extended_record_ref_norm({{record, Name}, Field})
  when is_atom(Name), is_atom(Field) ->
    extended_record_ref_norm({{record, Name}, [Field]});    
extended_record_ref_norm({{record, Name}, Fields} = RecRef)
  when is_atom(Name), is_list(Fields) ->
    case lists:all(fun is_atom/1, Fields) of
        true ->
            [ {{record,Name}, Field} || Field <- Fields ];
        false ->
            meta:error(?MODULE, {invalid_record_ref, RecRef})
    end;
extended_record_ref_norm({record, Name}) when is_atom(Name) ->
    [{record, Name}];
extended_record_ref_norm(Inv) ->
    meta:error(?MODULE, {invalid_record_ref, Inv}).

grounded_type_ref_norm({TypeName, TypeArgs})
  when is_atom(TypeName), is_list(TypeArgs) ->
    {TypeName, [ extended_type_ref_norm(A) || A <- TypeArgs ]};
grounded_type_ref_norm({record, RecName}) ->
    {record, RecName};    
grounded_type_ref_norm(Atom) when is_atom(Atom) ->
    {atom, Atom};
grounded_type_ref_norm(Inv) ->
    meta:error(?MODULE, {invalid_type_ref, Inv}).


grounded_to_typedef({record, Name}) when is_atom(Name) ->
    {record,[{atom,Name}]};
grounded_to_typedef({atom, Name}) when is_atom(Name) ->
    {atom, Name};
grounded_to_typedef({TypeName, TypeArgs})
  when is_atom(TypeName), is_list(TypeArgs) ->
    {TypeName, [ grounded_to_typedef(A) || A <- TypeArgs ]};
grounded_to_typedef(Inv) ->
    meta:error(?MODULE, {invalid_grounded_type_ref, Inv}).

code_fun_norm({Mod, FunName} = FunRef)
  when is_atom(Mod) andalso is_atom(FunName) ->
    FunRef;
code_fun_norm(FunName) when is_atom(FunName) ->
    FunName;
code_fun_norm(Inv) ->
    meta:error(?MODULE, {invalid_code_fun_reference, Inv}).


-spec format_error(any()) -> iolist().
format_error({invalid_coder_option, Inv}) ->
    io_lib:format("Invalid encoder/parser option: ~p", [Inv]);
format_error({invalid_name_fun_ref, Inv}) ->
    io_lib:format("Invalid record field name function spec: ~p", [Inv]);
format_error({invalid_surrogate_option, Inv}) ->
    io_lib:format("Invalid surrogate type option: ~p", [Inv]);
format_error({invalid_type_ref, Inv}) ->
    io_lib:format("Invalid extended type reference: ~p", [Inv]);
format_error({invalid_grounded_type_ref, Inv}) ->
    io_lib:format("Invalid type reference: ~p", [Inv]);
format_error({invalid_code_fun_ref, Inv}) ->
    io_lib:format("Invalid encode/decode function spec: ~p", [Inv]);
format_error({invalid_record_ref, RecRef}) ->
    io_lib:format("Invalid record type reference: ~p", [RecRef]).

