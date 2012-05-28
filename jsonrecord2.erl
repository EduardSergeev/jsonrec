-module(jsonrecord2).

-include("meta.hrl").
-include("meta_syntax.hrl").

-export([encode_gen/1]).

%%-compile(export_all).

%-record(stub, {id, field}).

encode_gen({Name, Fs}) ->
    {'fun', _, {clauses, Cs}} = subs(Name, Fs),
    {function, 0, encode, 1, Cs}.


%%encode(#stub{} = Record) ->
%%    {struct, stub}.

    
encode_fields(Fs, Record) ->
    NFs = lists:zip(lists:seq(2,length(Fs)+1), Fs),
    Es = [encode_field(Fn, I, Record) ||
             {I, {_, _, #atom{name = Fn}}} <- NFs],
    lists:foldr(fun cons/2, meta:quote([]), Es).

encode_field(FN, Ind, Rec) ->
    AFN = erl_parse:abstract(FN),
    AInd = erl_parse:abstract(Ind),
    meta:quote({AFN, element(AInd, Rec)}).

cons(H,T) ->
    meta:quote([H|T]).

%%t1() ->
%%    meta:reify(fun encode/1).


%% sample(#stub{} = Rec) ->
%%     {struct,
%%      [{id,element(2,Rec)},
%%       {field,element(2,Rec)}]}.

%% ts() ->
%%     meta:reify(fun sample/1).


%% fs() ->
%%     meta:reify(#stub{}).

%% st() ->
%%     meta:reify(fun encode/1).

subs(Name, Fs) ->
    Var = meta:quote(Rec),
    Record = {record, 0, Name, []},
    Pat = meta:quote(Record = Var),
    EFs = encode_fields(Fs, Var),
    meta:quote(
      fun(Pat) ->
              mochijson2:encode(
                {struct, EFs})
      end).

