-module(sample).

-compile({parse_transform, meta}).


-meta([sample_expr/0, sample_expr/1]).
-export([test/0, sample_expr/0, test_loc/0, sample_fun/0]).
-export([test_fun2/0]).
-export([test_fun3/0]).
-export([test_fun4/0]).
-export([sample_res/0]).
-export([sample_expr/1]).


sample_expr() ->
    meta:quote(2+20).

sample_fun() ->
    meta:quote(fun(A) -> A+1 end). 
	     

sample_expr(Arg1) ->
    meta:quote(Arg1+42).

test() ->
    meta:splice(sample2:expr()).

test_loc() ->
    meta:splice(sample_expr()).

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


sample_res() ->
    sample_expr({integr,5,23}).
