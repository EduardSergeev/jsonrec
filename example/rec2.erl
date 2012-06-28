-module(rec2).

-include("../src/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen/3]).
-meta([encode_gen/3]).

-record(rec0, {id :: [integer()]}).
-record(rec1,
        {id :: integer(),
         fi = <<>> :: binary()}).

-record(rec2,
        {id :: integer(),
         rec0 :: #rec0{},
         arr = [] :: [integer()],
         rec1 :: [#rec1{}]}).

%%-record(rec1, {id = 5 :: integer()}).

to_struct(#rec0{} = Rec) ->
    encode_gen(
      Rec,
      {record,rec0},
      meta:reify()).

%% to_struct2(#rec1{} = Rec) ->
%%     encode_gen(
%%       Rec,
%%       {record,rec1},
%%       meta:reify()).

to_struct3(#rec2{} = Rec) ->
    encode_gen(
      Rec,
      {record,rec2},
      meta:reify()).


%% sample_encode(#rec1{} = Rec) ->
%%     Fun0 = fun(Some) ->
%%                    Some
%%            end,
%%     Fun0(Rec).
                   

%% encode(Rec) ->
%%     mochijson2:encode(to_struct(Rec)).


%% t1() ->
%%     meta:reify_type(#rec1{}).

%% t2() ->
%%     meta:reify().
