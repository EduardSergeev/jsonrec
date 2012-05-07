
-module(meta).

-export([parse_transform/2]).
-export([format_error/1]).


-record(info,
       {meta = [],
	functions = dict:new()}).


-record(function, {line, name, arity, clauses}).
-record(call, {line, function, args}).
-record(remote, {line, module, name}).
-record(var, {line, name}).
-record(atom, {line, name}).

parse_transform(Forms, _Options) ->
    {Forms1, _} = mapfoldl(quote2(), undefined, Forms),
    Info = lists:foldl(fun do_trans/2, #info{}, Forms1),
    io:format("~p~n", [Info]),
    Forms2 = convert(splice(Info), Forms1),
    io:format("~p~n", [Forms2]),
    Forms2.

do_trans({attribute, _Line, meta, Meta}, Info) ->	    
    add_meta(Meta, Info);
do_trans({function, _Line, Name, Arity, Clauses}, Info) ->
    add_function({Name,Arity}, Clauses, Info);
do_trans(_Form, Info) ->	
    Info.


add_meta(Meta, #info{meta = Ms} = Info) ->
    Info#info{meta = [Meta|Ms]}.

add_function({Name,Arity}, Clauses, #info{functions = Fs} = Info) ->
    Info#info{functions = dict:store({Name,Arity}, Clauses, Fs)}.
    

splice(#info{functions = Fs}) ->
    fun({call, _Ln, {remote, _,{atom, _, meta}, {atom, _, splice}}, Splice}) ->
	    Local = fun(Name, Args) ->
			    Call = local_call(Fs, Name, Args),
			    {value, Val, _} = erl_eval:expr(Call, []),
			    Val
		    end,
	    {value, Val, _} = erl_eval:exprs(Splice, [], {value, Local}),
	    Val;
       (Form) ->
	    Form
    end.

quote() ->
    fun({call, _Line, {remote, _,{atom, _, meta}, {atom, _, quote}}, [Quote]}) ->
	    Ast = term_to_ast(Quote),
	    erl_syntax:revert(Ast);
       (Form) ->
	    Form
    end.

quote2() ->
    fun(#function{} = Fun, _) ->
	    {Fun, gb_sets:new()};
       (#call{function = #remote{module = {atom,_,meta}, name = {atom,_,quote}}, args = [Quote]}, Vars) ->
	    Ast = term_to_ast(Vars, Quote),
	    {erl_syntax:revert(Ast), Vars};
       (#var{name = Name} = Var, Vars) ->
	    {Var, gb_sets:add(Name, Vars)};
       (Form, Vars) ->
	    {Form, Vars}		       
    end.

local_call(Fs, FunName, Args) ->
    Cs = dict:fetch({FunName,length(Args)}, Fs),
    F = erl_syntax:fun_expr(Cs),
    A = erl_syntax:application(F, Args),
    erl_syntax:revert(A).
    

term_to_ast(Vars, Ls) when is_list(Ls) ->
    Al = lists:map(fun(L) -> term_to_ast(Vars, L) end, Ls),
    erl_syntax:list(Al);
term_to_ast(Vars, #var{name = Name} = A) ->
    case gb_sets:is_member(Name, Vars) of
	true ->
	    A;
	false ->
	    Ls = tuple_to_list(A),
	    Al = lists:map(fun(L) -> term_to_ast(Vars, L) end, Ls),
	    erl_syntax:tuple(Al)
    end;
term_to_ast(Vars, Tp) when is_tuple(Tp) ->
    Ls = tuple_to_list(Tp),
    Al = lists:map(fun(L) -> term_to_ast(Vars, L) end, Ls),
    erl_syntax:tuple(Al);
term_to_ast(_, I) when is_integer(I) ->
    erl_syntax:integer(I);
term_to_ast(_, A) when is_atom(A) ->
    erl_syntax:atom(A).

term_to_ast(Ls) when is_list(Ls) ->
    Al = lists:map(fun term_to_ast/1, Ls),
    erl_syntax:list(Al);
term_to_ast(Tp) when is_tuple(Tp) ->
    Ls = tuple_to_list(Tp),
    Al = lists:map(fun term_to_ast/1, Ls),
    erl_syntax:tuple(Al);
term_to_ast(I) when is_integer(I) ->
    erl_syntax:integer(I);
term_to_ast(A) when is_atom(A) ->
    erl_syntax:atom(A).


    
    

%%%
%%% Utilities
%%%
%%id(X) ->
%%    X.

traverse(Fun, [F|Fs]) ->
    case t_step(Fun, F) of
	nothing ->
	    traverse(Fun, Fs);
	F1 ->
	    [F1|traverse(Fun, Fs)]
    end;
traverse(_Fun, []) ->
    [];
traverse(_Fun, Smt) ->
    Smt.

t_step(Fun, Form) when is_tuple(Form) ->
    [Tag|Args] = tuple_to_list(Form),
    Args1 = lists:map(
	      fun(E) -> traverse(Fun, E) end,
	      Args),
    Fun(list_to_tuple([Tag|Args1])).


mapfoldl(Fun, Acc, Fs) when is_list(Fs) ->
    Step = fun(Form, Accum) ->
		   [Tag|Args] = tuple_to_list(Form),
		   {Args1,Accum1} = lists:mapfoldl(
				    fun(E, A) -> mapfoldl(Fun, A, E) end,
				    Accum, Args),
		   Fun(list_to_tuple([Tag|Args1]), Accum1)
	   end,
    lists:mapfoldl(Step, Acc, Fs);
mapfoldl(Fun, Acc, Smt) ->
    Fun(Smt, Acc).

convert(Fun, Forms) ->
    Do = fun(F) ->
		 try
		     t_step(Fun, F)
		 catch
		     error:{Line, Reason} ->
			 {error, {Line, ?MODULE, Reason}}
		 end
	 end,	 
    lists:map(Do, Forms).
		 


%% flatmap(Fun, [F|Fs]) ->
%%     m_step(Fun, F) ++ flatmap(Fun, Fs);
%% flatmap(_Fun, []) ->
%%     [];
%% flatmap(_Fun, Smt) ->
%%     Smt.


%% m_step(Fun, Form) when is_tuple(Form) ->
%%     [Tag|Args] = tuple_to_list(Form),
%%     Args1 = lists:map(
%% 	      fun(E) -> flatmap(Fun, E) end,
%% 	      Args),
%%     Fun(list_to_tuple([Tag|Args1])).

    
    
-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.
