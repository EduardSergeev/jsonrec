-module(jsonrecord2).

-include("meta.hrl").
-include("meta_syntax.hrl").

-export([encode_gen/1]).

encode_gen({Name, Fs}) ->
    {'fun', _, {clauses, Cs}} = subs(Name, Fs),
    {function, 0, encode, 1, Cs}.
    
encode_fields(Fs, Record) ->
    NFs = lists:zip(lists:seq(2,length(Fs)+1), Fs),
    Es = [encode_field(Fn, I, Record) ||
             {I, {_, _, #atom{name = Fn}}} <- NFs],
    Cons = fun(H,T) -> meta:quote([H|T]) end,
    lists:foldr(Cons, meta:quote([]), Es).

encode_field(FN, Ind, Rec) ->
    AFN = erl_parse:abstract(FN),
    AInd = erl_parse:abstract(Ind),
    meta:quote({AFN, element(AInd, Rec)}).

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

