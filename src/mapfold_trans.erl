
-module(mapfold_trans).

%% A identity transformer of Erlang abstract syntax.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for tranforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2]).

-export([encode_sample_record/1]).

-record(info,
       {types = dict:new(),
	record_types = dict:new(),
	records = dict:new(),
	jsons = []}).

-record(sample_record,
       {foo = false :: boolean(),
	boo :: integer(),
	koo = roo :: atom()}).

parse_transform(Forms, _Options) ->
    {Forms1, Acc1} = lists:mapfoldl(fun do_trans/2, #info{}, Forms),
    io:format("~p~n", [Acc1]),
    Forms1.



do_trans({attribute, _Line, type, Type} = Form, Info) ->	    
    {Form, add_type(Info, Type)};
do_trans({attribute, _Line, record, Record} = Form, Info) ->	
    {Form, add_record(Info, Record)};
do_trans({attribute, _Line, json, RecName} = Form, Info) ->	
    {Form, add_json(Info, RecName)};
do_trans(Form, Info) ->	
    {Form, Info}.


add_type(#info{record_types = Ts} = Info, {{record,Name}, Def, Params}) ->
    Info#info
	{record_types =
	     dict:append(Name, {Def, Params}, Ts)};
add_type(#info{types = Ts} = Info, {Name, Def, Params}) ->
    Info#info
	{types =
	     dict:append(Name, {Def, Params}, Ts)}.
    
add_record(#info{records = Rs} = Info, {Name, Def}) ->
    Info#info{records = dict:append(Name, Def, Rs)}.

add_json(#info{jsons = Js} = Info, RecName) ->
    Info#info{jsons = [RecName|Js]}.


%%gen_encode(Info, RecName) ->
%%    


encode_boolean(true) ->
    <<"true">>;
encode_boolean(false) ->
    <<"false">>;
encode_boolean(Inv) ->
    error({unexpected_value, Inv}).

encode_integer(Integer) when is_integer(Integer) ->
    list_to_binary(integer_to_list(Integer));
encode_integer(Inv) ->
    error({unexpected_value, Inv}).

encode_atom(Atom) when is_atom(Atom) ->    
    [$",atom_to_binary(Atom, utf8),$"];
encode_atom(Inv) ->
    error({unexpected_value, Inv}).



encode_sample_record(#sample_record{} = Rec) ->
    S0 = [],
    S1 = add_field(encode_atom(koo), encode_atom(Rec#sample_record.koo), S0),
    S2 = if
	     Rec#sample_record.boo =/= undefined ->
		 add_field(encode_atom(boo), encode_integer(Rec#sample_record.boo), S1);
	     true ->
		 S1
	 end,
    S3 = add_field(encode_atom(foo),encode_boolean(Rec#sample_record.foo), S2),
    [<<"{">>,S3,<<"}">>].

add_field(Key, Value, []) ->
    [[Key,<<":">>,Value]|[]];
add_field(Key, Value, Acc) ->
    [[Key,<<":">>,Value,<<",">>]|Acc].

    
