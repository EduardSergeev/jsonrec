-module(sample3).

-include("../src/meta.hrl").

-compile(export_all).

-record(rec1,
        {id :: integer(),
         field :: binary()}).

-record(rec2,
        {id :: integer(),
         field :: [boolean()],
         ref :: #rec1{}}).

-import(sample2, [do/1]).

-meta([do/1]).

-splice(mk_rec_foo).

-type my_type(A,B) :: {A,[B]}.


-spec f1() -> any().
f1() ->
    meta:quote(1+2).

f2() ->
    meta:reify(fun f1/0).

f3() ->
    meta:reify(#rec1{}).



f4() ->
    meta:reify_type(my_type(_,_)).

f5() ->
    meta:reify_type(fun f1/0).

f6() ->
    meta:reify_type(#rec1{}).

mk_rec(Name) ->
    R = meta:reify(#rec1{}),
    {_, Fs} = R,
    {attribute, 0, record,
     {Name, Fs}}.

mk_rec_foo() ->
    mk_rec(foo).

%%gg() ->
%%    F = #foo{id = 5},
%%    F.
