
-module(meta).

-export([parse_transform/2]).
-export([format_error/1]).

-record(info,
       {meta = [],
        functions = dict:new()}).


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
-define(SPLICE(Ln, Var), ?META_CALL(Ln, splice, Var)).


parse_transform(Forms, _Options) ->
    Forms1 = form_traverse(fun quote/2, undefined, Forms),
    {_, Info} = traverse(fun info/2, #info{}, Forms1),
    Forms2 = form_traverse(fun splice/2, Info#info.functions, Forms1),
%%    io:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms2))]),
    Forms2.


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
    throw({Ln, "meta:quote/1 is not allowed within another meta:quote/1"});
term_to_ast(?SPLICE(Ln, _), _) ->
    throw({Ln, "meta:splice/1 is not allowed within meta:quote/1"});
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
%% Various info gathering for subsequent use
%%
info(#attribute{name = meta, arg = Meta} = Form,
     #info{meta = Ms} = Info) ->
    Info1 = Info#info{meta = [Meta|Ms]},
    {Form, Info1};
info(#function{ name = Name, arity = Arity, clauses = Clauses} = Form,
     #info{functions = Fs} = Info) ->
    Info1 = Info#info{functions = dict:store({Name,Arity}, Clauses, Fs)},
    {Form, Info1};
info(Form, Info) ->
    traverse(fun info/2, Info, Form).
    

%%
%% mete:splice/1 handling
%%
splice(?SPLICE(_, Splice), Fs) ->
    Local = fun(Name, Args) ->
                    Call = local_call(Fs, Name, Args),
                    {value, Val, _} = erl_eval:expr(Call, []),
                    Val
            end,
    {value, Val, _} = erl_eval:exprs(Splice, [], {value, Local}),
    {Val, Fs};
splice(Form, Fs) ->
    traverse(fun splice/2, Fs, Form).

local_call(Fs, FunName, Args) ->
    Cs = dict:fetch({FunName,length(Args)}, Fs),
    F = erl_syntax:fun_expr(Cs),
    A = erl_syntax:application(F, Args),
    erl_syntax:revert(A).


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

traverse(Fun, Acc, Form) when is_tuple(Form) ->
    Fs = tuple_to_list(Form),
    {Fs1, Acc1} = traverse(Fun, Acc, Fs),
    {list_to_tuple(Fs1), Acc1};
traverse(Fun, Acc, Fs) when is_list(Fs) ->
    lists:mapfoldl(Fun, Acc, Fs);
traverse(_Fun, Acc, Smt) ->
    {Smt, Acc}.


-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.
