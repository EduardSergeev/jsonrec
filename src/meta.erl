
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
%% -record(atom, {line, name}).

-record(q_state, {in_quote, vars}).


-define(META_CALL(Name, Args),
	#call
	{function = #remote
	 {module = {atom,_,meta},
	  name = {atom,_,Name}},
	 args = Args}).
-define(QUOTE(Var), ?META_CALL(quote, [Var])).
-define(SPLICE(Var), ?META_CALL(splice, Var)).


parse_transform(Forms, _Options) ->
    Forms1 = mapforms(
	       fun quote_before/2,
	       fun quote_after/2,
	       #q_state{}, Forms),
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
    fun(?SPLICE(Splice)) ->
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

splice_before(#info{functions = Fs}) ->
    fun(?SPLICE(Splice)) ->
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


local_call(Fs, FunName, Args) ->
    Cs = dict:fetch({FunName,length(Args)}, Fs),
    F = erl_syntax:fun_expr(Cs),
    A = erl_syntax:application(F, Args),
    erl_syntax:revert(A).


quote_before(#function{}, _) ->
    #q_state{vars = gb_sets:new(), in_quote = false};
quote_before(?QUOTE(_), #q_state{in_quote = false} = QS) ->
    QS#q_state{in_quote = true};
quote_before(?QUOTE(_), #q_state{in_quote = true}) ->
    error(nested_quote);

quote_before(#var{name = Name}, #q_state{in_quote = false, vars = Vs} = QS) ->
    QS#q_state{vars = gb_sets:add(Name, Vs)};
quote_before(#var{name = Name}, #q_state{in_quote = true, vars = Vs} = QS) ->
    QS#q_state{vars = gb_sets:add(Name, Vs)};

quote_before(_, QS) ->
    QS.

quote_after2(?QUOTE(Ast), QS) ->
    {erl_syntax:revert(Ast), QS#q_state{in_quote = false}};
quote_after2(#var{name = Name} = A, #q_state{in_quote = true, vars = Vs}) ->
    case gb_sets:is_member(Name, Vs) of
	true ->
	    A;
	false ->
	    Ls = tuple_to_list(A),
	    Al = lists:map(fun(L) -> term_to_ast(Vs, L) end, Ls),
	    erl_syntax:tuple(Al)
    end;

quote_after2(Form, QS) ->
    {Form, QS}.


quote_after(?QUOTE(Quote), QS) ->
    Ast = term_to_ast(QS#q_state.vars, Quote),
    {erl_syntax:revert(Ast), QS#q_state{in_quote = false}};
quote_after(Form, QS) ->
    {Form, QS}.



term_to_ast(Vs, Ls) when is_list(Ls) ->
    Al = lists:map(fun(L) -> term_to_ast(Vs, L) end, Ls),
    erl_syntax:list(Al);
term_to_ast(Vs, #var{name = Name} = A) ->
    case gb_sets:is_member(Name, Vs) of
	true ->
	    A;
	false ->
	    Ls = tuple_to_list(A),
	    Al = lists:map(fun(L) -> term_to_ast(Vs, L) end, Ls),
	    erl_syntax:tuple(Al)
    end;
term_to_ast(Vs, Tp) when is_tuple(Tp) ->
    Ls = tuple_to_list(Tp),
    Al = lists:map(fun(L) -> term_to_ast(Vs, L) end, Ls),
    erl_syntax:tuple(Al);
term_to_ast(_, I) when is_integer(I) ->
    erl_syntax:integer(I);
term_to_ast(_, A) when is_atom(A) ->
    erl_syntax:atom(A).

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


mapforms(FB, FA, Acc, Forms) ->
    Fun = fun(F) ->
		  try
		      {F1, _} = mapfoldl(FB, FA, Acc, F),
		      F1
		  catch
		      error:{Ln, Err} ->
			  {error, {Ln, ?MODULE, Err}}
		  end
	  end,	 
    lists:map(Fun, Forms).    
		 

mapfoldl(FB, FA, Acc, Fs) when is_list(Fs) ->
    Fun = fun(E,A) ->
		  mapfoldl(FB, FA, A, E)
	  end,
    lists:mapfoldl(Fun, Acc, Fs);
mapfoldl(FB, FA, Acc, Form) when is_tuple(Form) ->
    Acc1 = FB(Form, Acc),
    [Tag|Fs] = tuple_to_list(Form), 
    {Fs1,Acc2} = mapfoldl(FB, FA, Acc1, Fs),
    FA(list_to_tuple([Tag|Fs1]), Acc2);
mapfoldl(FB, FA, Acc, Smt) ->
    Acc1 = FB(Smt, Acc),
    FA(Smt, Acc1).


    
-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.
