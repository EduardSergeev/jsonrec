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

-type my_type(A,B) :: {A,[B]}.

f1() ->
    meta:quote(1+2).

f2() ->
    meta:reify(fun f1/0).

-splice(f2).
