
-module(meta).

-export([parse_transform/2]).
-export([format_error/1]).

-record(info,
        {meta = [],
         records = dict:new(),
         functions = dict:new()}).

-record(splice, {vars, funs}).

-record(attribute, {line, name, arg}).
-record(function, {line, name, arity, clauses}).
-record(call, {line, function, args}).
-record(remote, {line, module, name}).
-record(var, {line, name}).
-record(atom, {line, name}).


-define(META_CALL(Ln, Name, Args),
        #call{line = Ln,
              function = #remote
              {module = #atom{name = meta},
               name = #atom{name = Name}},
              args = Args}).
-define(QUOTE(Ln, Var), ?META_CALL(Ln, quote, [Var])).
-define(REIFY(Ln, Name), ?META_CALL(Ln, reify, [Name])).
-define(SPLICE(Ln, Var), ?META_CALL(Ln, splice, Var)).


parse_transform(Forms, _Options) ->
    io:format("~p", [Forms]),
    {_, PredInfo} = traverse(fun info/2, #info{}, Forms),
    Forms1 = form_traverse(fun reify/2, PredInfo, Forms),    

    Forms2 = form_traverse(fun quote/2, undefined, Forms1),
    {_, Info} = traverse(fun info/2, #info{}, Forms2),
    Fs = Info#info.functions,

    Forms3 = form_traverse(fun splice/2, #splice{funs = Fs}, Forms2),
    io:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms3))]),
    Forms3.


%%
%% meta:quote/1 handling
%%
quote(#function{} = Func, _) ->
    traverse(fun quote/2, gb_sets:new(), Func);
quote(?QUOTE(_, Quote), Vs) ->
    {Ast, _} = term_to_ast(Quote, Vs),
    {erl_syntax:revert(Ast), Vs};
quote(#var{name = Name} = V, Vs) ->
    {V, gb_sets:add(Name, Vs)};
quote(Form, Vs) ->
    traverse(fun quote/2, Vs, Form).

term_to_ast(?QUOTE(Ln, _), _) ->
    meta_error(Ln, nested_quote);
term_to_ast(?SPLICE(Ln, _), _) ->
    meta_error(Ln, nested_splice);
term_to_ast(Ls, Vs) when is_list(Ls) ->
    {Ls1, _} = traverse(fun term_to_ast/2, Vs, Ls),
    {erl_syntax:list(Ls1), Vs};
term_to_ast(#var{name = Name} = A, Vs) ->
    case gb_sets:is_member(Name, Vs) of
        true ->
            {A, Vs};
        false ->
            tuple_to_ast(A, Vs)
    end;
term_to_ast(T, Vs) when is_tuple(T) ->
    tuple_to_ast(T, Vs);
term_to_ast(I, Vs) when is_integer(I) ->
    {erl_syntax:integer(I), Vs};
term_to_ast(A, Vs) when is_atom(A) ->
    {erl_syntax:atom(A), Vs}.    
    
tuple_to_ast(T, Vs) ->
    Ls = tuple_to_list(T),
    {Ls1, _} = traverse(fun term_to_ast/2, Vs, Ls),
    {erl_syntax:tuple(Ls1), Vs}.


%%
%% meta:reify/1 handling
%%
reify(?REIFY(Ln, {'fun', _, {function, Name, Arity}}),
      #info{functions = Fs} = Info) ->
    Fn = {Name, Arity},
    case dict:find(Fn, Fs) of
        {ok, Cs} ->
            {Ast, _} = term_to_ast(Cs, gb_sets:new()),
            {erl_syntax:revert(Ast), Info};
        error ->
            meta_error(Ln, {reify_unknown_function, Fn})
    end;            
reify(?REIFY(Ln, {record, _, Name, []}),
      #info{records = Rs} = Info) ->
    case dict:find(Name, Rs) of
        {ok, Df} ->
            {Ast, _} = term_to_ast(Df, gb_sets:new()),
            {erl_syntax:revert(Ast), Info};
        error ->
            meta_error(Ln, {reify_unknown_record, Name})
    end;            
    
reify(Form, Info) ->
    traverse(fun reify/2, Info, Form).


%%
%% Various info gathering for subsequent use
%%
info(#attribute{name = meta, arg = Meta} = Form,
     #info{meta = Ms} = Info) ->
    Info1 = Info#info{meta = [Meta|Ms]},
    {Form, Info1};
info(#attribute{name = record, arg = {Name, _} = Def} = Form,
     #info{records = Rs} = Info) ->
    Rs1 = dict:store(Name, Def, Rs),
    Info1 = Info#info{records = Rs1},
    {Form, Info1};
info(#function{ name = Name, arity = Arity} = Form,
     #info{functions = Fs} = Info) ->
    Info1 = Info#info{functions = dict:store({Name,Arity}, Form, Fs)},
    {Form, Info1};
info(Form, Info) ->
    traverse(fun info/2, Info, Form).
    

%%
%% mete:splice/1 handling
%%
splice(#function{} = Func, S) ->
    traverse(fun splice/2, S#splice{vars = gb_sets:new()}, Func);
splice(?SPLICE(Ln, Splice), #splice{funs = Fs} = S) ->
    {eval_splice(Ln, Splice, Fs), S};
splice(Form, S) ->
    traverse(fun splice/2, S, Form).

eval_splice(Ln, Splice, Fs) ->
    Local = local_handler(Ln, Fs),
    try
        {value, Val, _} = erl_eval:exprs(Splice, [], {eval, Local}),
        Val
    catch
        error:{unbound, Var} ->
            meta_error(Ln, splice_external_var, Var);
        error:{badarity, _} ->
            meta_error(Ln, splice_badarity);
        error:{badfun, _} ->
            meta_error(Ln, splice_badfun);
        error:_ ->
            meta_error(Ln, invalid_splice)
    end.

local_handler(Ln, Fs) ->
    fun(Name, Args, Bs) ->
            Fn = {Name, length(Args)},
            case dict:find(Fn, Fs) of
                {ok, #function{clauses = Cs}} ->
                    F = erl_syntax:fun_expr(Cs),
                    A = erl_syntax:application(F, Args),
                    Call = erl_syntax:revert(A),
                    erl_eval:expr(Call, Bs, {eval, local_handler(Ln, Fs)});
                error ->
                    meta_error(Ln, {splice_unknown_function, Fn})
            end
    end.


%%
%% Recursive traversal a-la mapfoldl
%%
traverse(Fun, Acc, Form) when is_tuple(Form) ->
    Fs = tuple_to_list(Form),
    {Fs1, Acc1} = traverse(Fun, Acc, Fs),
    {list_to_tuple(Fs1), Acc1};
traverse(Fun, Acc, Fs) when is_list(Fs) ->
    lists:mapfoldl(Fun, Acc, Fs);
traverse(_Fun, Acc, Smt) ->
    {Smt, Acc}.

form_traverse(Fun, Acc, Forms) ->
    Do = fun(F, A) ->
                 try
                     Fun(F, A)
                 catch
                     throw:{Line, Reason} ->
                         {{error, {Line, ?MODULE, Reason}}, A};
                     throw:{Line, Reason, A1} ->
                         {{error, {Line, ?MODULE, Reason}}, A1}
                 end
         end,    
    {Forms1, _} = lists:mapfoldl(Do, Acc, Forms),
    Forms1.


meta_error(Line, Error) ->
    throw({Line, Error}).

meta_error(Line, Error, Arg) ->
    throw({Line, {Error, Arg}}).


format_error(nested_quote) ->
    "meta:quote/1 is not allowed within another meta:quote/1";
format_error(nested_splice) ->
    "meta:splice/1 is not allowed within meta:quote/1";
format_error({reify_unknown_function, {Name, Arity}}) ->
    format("attempt to reify unknown function '~s/~b'", [Name, Arity]);
format_error({reify_unknown_record, Name}) ->
    format("attempt to reify unknown record '~s'", [Name]);
format_error(invalid_splice) ->
    "invalid expression in meta:splice/1";
format_error({splice_external_var, Var}) ->
    format("Variable '~s' is outside of scope of meta:splice/1", [Var]);
format_error(splice_badarity) ->
    "'badarity' call in 'meta:splice'";
format_error(splice_badfun) ->
    "'badfun' call in 'meta:splice'";
format_error({splice_unknown_function, {Name,Arity}}) ->
    format("Unknown local function '~s/~b' used in 'meta:splice/1'", [Name,Arity]).
    
format(Format, Args) ->
    io_lib:format(Format, Args).
    
