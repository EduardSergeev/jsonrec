-module(sample).

-include("../src/meta.hrl").

-meta([sample_expr/0, sample_expr/1]).

-compile(export_all).

sample_expr() ->
    meta:quote(1+20).

sample_fun() ->
    meta:quote(fun(A) -> A+1 end). 
	     

sample_expr(Arg1) ->
    meta:quote(meta:splice(Arg1) + 42).

test() ->
    meta:splice(sample2:expr()).

test_loc() ->
    sample_expr().

test_fun2() ->
    meta:splice(sample2:expr(meta:quote(2))).

test_fun3() ->
    G = meta:splice(meta:quote(5+6)),
    G.

test_fun4() ->
    F = meta:splice(sample_fun()),
    F(1).

%%sm() ->
%%    [{clause,15,[],[],
%%      [{op,15,'+',{var,15,'Arg1'},{integer,15,42}}]}].

%%res() ->
%%    A = [1,2],
%%    lists:sum(A).

sample_splice() ->
    {'fun', Ln,{clauses, Cs}} = sample_fun(),
    {function, Ln, eee_fun, 1, Cs}.

%%-splice({{sample2,r1},[]}).

%%-splice(sample_splice).

sample_res() ->
    sample_expr({integer,5,23}).
