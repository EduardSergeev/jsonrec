-module(jsonrecord2).

-include("meta.hrl").
-include("meta_syntax.hrl").

%% -export([encode_gen_ms/4, decode_gen_ms/4,
%%          encode_gen/3]).

-export([encode_gen/3]).

-define(FIELD(Name),
        {record_field, _Ln1,
         {atom, _Ln2, Name}}).
-define(FIELD(Name, Default),
        {record_field, _Ln1,
         {atom, _Ln2, Name},
         Default}).
-define(TYPED_FIELD(Name, Type, Args),
        {typed_record_field,
         ?FIELD(Name),
         {type, _Ln3, Type, Args}}).
-define(TYPED_FIELD(Name, Type, Args, Default),
        {typed_record_field,
         ?FIELD(Name, Default),
         {type, _Ln3, Type, Args}}).



%% encode_gen_ms(Rec,
%%                {Name, Fields},
%%                {{record, Name}, TypeDefs, []},
%%                Mps) ->
%%     EFs = encode_fields_ms(Rec, Fields, TypeDefs, Mps),
%%     meta:quote({struct, EFs}).


encode_gen(Item, {record,RecordName}, Info) ->
    Type = {record, [{atom,0,RecordName}]},
    {Fun,Mps} = gen_encode(Type, Info, []),
    %% Fs = [meta:quote(FN = Def)
    %%       || {_,{FN,Def}} <- Mps],
    Fs = lists:map(
           fun({_,{FN,Def}}) ->
                   meta:quote(FN = Def)
           end,
           lists:reverse(Mps)),
    AItem = meta:quote(Item),
    erl_syntax:block_expr(Fs ++ [Fun(AItem)]).
    
    

    


%% decode_gen_ms(Struct,
%%                {Name, Fields},
%%                {{record, Name}, TypeDefs, []},
%%                Mps) ->
%%     Size = length(Fields) + 1,
%%     AS = erl_parse:abstract(Size),
%%     FTI = gen_field_to_integer(Fields, TypeDefs, Mps),
%%     AFs = meta:quote(Fs),
%%     AFs1 = meta:quote([FTI(F,V) || {F,V} <- AFs]),
%%     AFs2 = meta:quote([T || T <- AFs1, is_tuple(T)]),
%%     AFs3 = with_defaults(Name, Fields, AFs2),
%%     meta:quote(
%%       case Struct of
%%           {struct, AFs} ->
%%               erlang:make_tuple(AS, undefined, AFs3)
%%       end).



%%
%% Encoding
%%
fetch_encode(Type, Info, Mps) ->
    case proplists:lookup(Type, Mps) of
        {Type, {Fun,_Def}} ->
            {Fun, Mps};
        none ->
            gen_encode(Type, Info, Mps)
    end.
    
gen_encode({record, [{atom, _, RecName}]} = Type, Info, Mps) ->
%%    Type = {record, RecName},
    {_, Fields, []} = meta:reify_type({record, RecName}, Info),
    ARec = meta:quote(Rec),
    {Def, Mps1} = encode_fields(ARec, Fields, Info, Mps),
    Def1 = meta:quote(
             fun(Rec) -> {struct, Def} end),
    add_fun_def(Type, Def1, Mps1);
gen_encode({list, [{type, _, Type, Args}]}, Info, Mps) ->
    {Fun, Mps1} = fetch_encode({Type, Args}, Info, Mps),
    AX = meta:quote(X),
    AFun = Fun(AX),
    Def = meta:quote(
            fun(Xs) ->
                    [AFun || X <- Xs]
            end),
    add_fun_def(Type, Def, Mps1);
gen_encode({union, [{atom, _, undefined}, {type, _, Type, Args}]}, Info, Mps) ->
    fetch_encode({Type, Args}, Info, Mps);

gen_encode({integer, []}, _Info, Mps) ->
    Fun = fun(Item) -> Item end,
    {Fun, Mps};
gen_encode({binary, []}, _Info, Mps) ->
    Fun = fun(Item) -> Item end,
    {Fun, Mps};
gen_encode({float, []}, _Info, Mps) ->
    Fun = fun(Item) -> Item end,
    {Fun, Mps};
gen_encode(Type, _Info, _Mps) ->
    meta:meta_error({unexpected_type, Type}).

                     

encode_fields(Rec, Fields, Info, Mps) ->
    NFs = lists:zip(lists:seq(2, length(Fields)+1), Fields),
    {Es,Mps1} = lists:mapfoldl(
                fun({I,T}, M) ->
                        encode_field(I, T, Rec, Info, M)
                end,
                Mps,
                NFs),
    Cons = fun(H,T) -> meta:quote(H(T)) end,
    {lists:foldr(Cons, meta:quote([]), Es),Mps1}.

encode_field(Ind, ?FIELD(Fn), Rec, Info, Mps) ->
    encode_typed(Ind, Fn, undefined, Rec, Info, Mps);
 encode_field(Ind, ?FIELD(Fn, _Def), Rec, Info, Mps) ->
    encode_typed(Ind, Fn, undefined, Rec, Info, Mps);
encode_field(Ind, ?TYPED_FIELD(Fn, T, Args), Rec, Info, Mps) ->
    encode_typed(Ind, Fn, {T,Args}, Rec, Info, Mps);
encode_field(Ind, ?TYPED_FIELD(Fn, T, Args, _Def), Rec, Info, Mps) ->
    encode_typed(Ind, Fn, {T,Args}, Rec, Info, Mps).



%% encode_fields_ms(Record, Fields, Types, Mps) ->
%%     NFs = lists:zip3(lists:seq(2, length(Fields)+1), Fields, Types),
%%     Es = [encode_field(I, F, T, Record, Mps) ||
%%              {I, F, T} <- NFs],
%%     Cons = fun(H,T) -> meta:quote(H(T)) end,
%%     lists:foldr(Cons, meta:quote([]), Es).


%% encode_field(Ind, {record_field, _Ln, #atom{name = Fn}} = RF,
%%               RF, Rec, _) ->
%%     encode_plain(Ind, Fn, Rec);
%% encode_field(Ind, {record_field, _Ln, #atom{name = Fn}, _Def} = RF,
%%               RF, Rec, _) ->
%%     encode_plain(Ind, Fn, Rec);

%% encode_field(Ind, {record_field, _Ln, #atom{name = Fn}} = RF,
%%               {typed_record_field, RF, T}, Rec, Mps) ->
%%     encode_typed(Ind, RF, Fn, T, Rec, Mps);
%% encode_field(Ind, {record_field, _Ln, #atom{name = Fn}, _Def} = RF,
%%               {typed_record_field, RF, T}, Rec, Mps) ->
%%     encode_typed(Ind, RF, Fn, T, Rec, Mps).

%% encode_plain(Ind, Fn, Rec) ->            
%%     AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
%%     AInd = erl_parse:abstract(Ind),
%%     meta:quote(
%%       fun(Acc) ->
%%           V = element(AInd, Rec),
%%           if 
%%               V =:= undefined ->
%%                   Acc;
%%               true ->
%%                   [{AFN, V}|Acc]
%%           end
%%       end).

encode_typed(Ind, Fn, Type, Rec, Info, Mps) ->
    {Fun, Mps1} = fetch_encode(Type, Info, Mps),
    AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
    AInd = erl_parse:abstract(Ind),
    AV = meta:quote(V),
    Elem = Fun(AV),
    Def = meta:quote(
            fun(Acc) ->
                    V = element(AInd, Rec),
                    if 
                        V =:= undefined ->
                            Acc;
                        true ->
                            [{AFN, Elem}|Acc]
                    end
            end),
    {Def, Mps1}.
    


%% encode_typed(Ind, Fn, record, [{atom,_ , Name}], Rec, Info, Mps) ->
%%     {Fun, Mps1} = get_fun({record,Name}, Info, Mps),
%%     {encode_record(Ind, Fn, Fun, Rec), Mps1};
%% encode_typed(Ind, Fn, list, [Type], Rec, Info, Mps) ->
%%     {Fun, Mps1} = get_fun({record,Name}, Info, Mps),
%%     {encode_record(Ind, Fn, Fun, Rec), Mps1};

%% encode_typed(Ind, Fn, Type, _Args, Rec, Info, Mps) ->
%%     {Fun, Mps1} = get_fun({record,Name}, Info, Mps),
%%     {encode_record(Ind, Fn, Fun, Rec), Mps1};

%%     case {T,TArgs} of
%%         {}
%%         {union,
%%          [_Und,
%%           {type, _, record, [{atom, _, Name}]}]} ->
%%             {Fun, Mps1} = get_fun(Name, Info, Mps),
%%             {encode_record(Ind, Fn, Fun, Rec), Mps1};
%%         {record,
%%          [{atom, _, Name}]} ->
%%             {Fun, Mps1} = get_fun(Name, Info, Mps),
%%             {encode_record(Ind, Fn, Fun, Rec), Mps1};
%%         {list,
%%          [{type, _, record, [{atom, _, Name}]}]} ->
%%             {Fun, Mps1} = get_fun(Name, Info, Mps),
%%             {encode_list(Ind, Fn, Fun, Rec), Mps1};
%%         {union,
%%          [_Und,
%%           {type, _, list,
%%            [{type, _, record, [{atom, _, Name}]}]}]} ->
%%             {Fun, Mps1} = get_fun(Name, Info, Mps),
%%             {encode_list(Ind, Fn, Fun, Rec), Mps1}
%%         %% _ ->
%%         %%     encode_field(Ind, RF, Rec, Info, Mps)
%%     end.


%% encode_typed(Ind, RF, Fn, T, Rec, Mps) ->
%%     case T of
%%         {type, _, union,
%%          [_Und, {type, _, record, [{atom, _, Name}]}]} ->
%%             Fun = get_fun(Name, Mps),
%%             encode_record(Ind, Fn, Fun, Rec);
%%         {type, _, record,
%%          [{atom, _, Name}]} ->
%%             Fun = get_fun(Name, Mps),
%%             encode_record(Ind, Fn, Fun, Rec);
%%         {type, _, list,
%%          [{type, _, record, [{atom, _, Name}]}]} ->
%%             Fun = get_fun(Name, Mps),
%%             encode_list(Ind, Fn, Fun, Rec);
%%         {type, _, union,
%%          [_Und,
%%           {type, _, list,
%%            [{type, _, record, [{atom, _, Name}]}]}]} ->
%%             Fun = get_fun(Name, Mps),
%%             encode_list(Ind, Fn, Fun, Rec);
%%         _ ->
%%             encode_field(Ind, RF, RF, Rec, Mps)
%%     end.


%% encode_record(Ind, Fn, Fun, Rec) ->
%%     AFun = erl_parse:abstract(Fun),
%%     AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
%%     AInd = erl_parse:abstract(Ind),
%%     meta:quote(
%%       fun(Acc) ->
%%               V = element(AInd, Rec),
%%               if 
%%                   V =:= undefined ->
%%                       Acc;
%%                   true ->
%%                       [{AFN, AFun(V)}|Acc]
%%               end
%%       end).

%% encode_list(Ind, Fn, Fun, Rec) ->
%%     AFun = erl_parse:abstract(Fun),
%%     AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
%%     AInd = erl_parse:abstract(Ind),
%%     meta:quote(
%%       fun(Acc) ->
%%               Vs = element(AInd, Rec),
%%               if 
%%                   Vs =:= undefined ->
%%                       Acc;
%%                   true ->
%%                       [{AFN, [AFun(V) || V <- Vs]}|Acc]
%%               end
%%       end).





%% %%
%% %% Decoding
%% %%
%% with_defaults(Name, Fs, Tail) ->
%%     NFs = lists:zip(lists:seq(2, length(Fs)+1), Fs),
%%     Ds = [decode_default(N, Def)
%%           || {N, {record_field, _, _, Def}} <- NFs],
%%     AN = erl_parse:abstract(Name),
%%     Tag = meta:quote({1,AN}),
%%     Ast = erl_syntax:list([Tag|Ds], Tail),
%%     erl_syntax:revert(Ast).    

%% gen_field_to_integer(Fields, Types, Mps) ->
%%     NFTs = lists:zip3(lists:seq(2, length(Fields)+1), Fields, Types),
%%     Es = [decode_field(N,F,T,Mps) || {N,F,T} <- NFTs],
%%     Last = erl_syntax:clause(
%%              [erl_syntax:underscore(),erl_syntax:underscore()],
%%              none,
%%              [meta:quote(undefined)]),
%%     Es1 = Es ++ [Last],
%%     Ast = erl_syntax:fun_expr(Es1),
%%     erl_syntax:revert(Ast).

%% decode_default(Ind, Def) ->
%%     AInd = erl_parse:abstract(Ind),
%%     meta:quote({AInd, Def}).

%% decode_field(Ind, {record_field, _, #atom{name = Fn}} = RF,
%%              RF, _) ->
%%     decode_plain(Ind, Fn);
%% decode_field(Ind,{record_field, _, #atom{name = Fn}, _Def} = RF,
%%              RF, _) ->
%%     decode_plain(Ind, Fn);

%% decode_field(Ind, {_, _, #atom{name = Fn}} = RF,
%%              {typed_record_field, RF, T}, Mps) ->
%%     decode_typed(Ind, RF, Fn, T, Mps);
%% decode_field(Ind, {_, _, #atom{name = Fn}, _Def} = RF,
%%              {typed_record_field, RF, T}, Mps) ->
%%     decode_typed(Ind, RF, Fn, T, Mps).

%% decode_plain(Ind, Fn) ->
%%     AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
%%     Var = meta:quote(V0),
%%     AInd = erl_parse:abstract(Ind),
%%     Res = meta:quote({AInd,Var}),
%%     erl_syntax:clause([AFN, Var], none, [Res]).

%% decode_typed(Ind, RF, Fn, T, Mps) ->   
%%     case T of
%%         {type, _, union,
%%          [_Und,
%%           {type, _, record, [{atom, _, Name}]}]} ->
%%             Fun = get_fun(Name, Mps),
%%             decode_record(Ind, Fn, Name, Fun);
%%         {type, _, record,
%%          [{atom, _, Name}]} ->
%%             Fun = get_fun(Name, Mps),
%%             decode_record(Ind, Fn, Name, Fun);
%%         {type, _, list,
%%          [{type, _, record, [{atom, _, Name}]}]} ->
%%             Fun = get_fun(Name, Mps),
%%             decode_list(Ind, Fn, Name, Fun);
%%         {type, _, union,
%%          [_Und,
%%           {type, _, list,
%%            [{type, _, record, [{atom, _, Name}]}]}]} ->
%%             Fun = get_fun(Name, Mps),
%%             decode_list(Ind, Fn, Name, Fun);
%%         _ ->
%%             decode_field(Ind, RF, RF, Mps)
%%     end.
    

%% decode_record(Index, Fn, Name, Fun) ->
%%     AFun = erl_parse:abstract(Fun),
%%     AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
%%     Var = meta:quote(V1),
%%     AInd = erl_parse:abstract(Index),
%%     Type = erl_parse:abstract(Name),
%%     Res = meta:quote({AInd,AFun(Type, Var)}),
%%     erl_syntax:clause([AFN, Var], none, [Res]).

%% decode_list(Index, Fn, Name, Fun) ->
%%     AFun = erl_parse:abstract(Fun),
%%     AFN = erl_parse:abstract(atom_to_msbinary(Fn)),
%%     Var = meta:quote(Vs),
%%     AInd = erl_parse:abstract(Index),
%%     Type = erl_parse:abstract(Name),
%%     Res = meta:quote({AInd,[AFun(Type, V2) || V2 <- Var]}),
%%     erl_syntax:clause([AFN, Var], none, [Res]).
            

    
%% get_fun(Type, Info, Mps) ->
%%     case proplist:lookup(Type, Mps) of
%%         {Type, Fun} ->
%%             {Fun, Mps};
%%         none ->
%%             ARec = meta:quote(Rec),
%%             {Body, Mps1} = gen_encode(ARec, Name, Info, Mps),
%%             Fun = meta:quote(
%%                     fun(Rec) ->
%%                             meta:splice(Body)
%%                     end),
%%             {Fun,[{Name,Fun}|Mps1]}
%%     end.

    
             
%%
%% Utils
%%
atom_to_mslist(Atom) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    Parts = string:tokens(List, "_"),
    Capitalized = lists:map(fun([H|T]) -> string:to_upper([H]) ++ T end, Parts),
    lists:concat(Capitalized). 

atom_to_msbinary(Atom) ->
    list_to_binary(atom_to_mslist(Atom)).


add_fun_def(Type, Def, Mps) ->
    Ind = length(Mps),
    VN = list_to_atom("Fun" ++ integer_to_list(Ind)),
    AFun = erl_syntax:revert(erl_syntax:variable(VN)),
    Fun = fun(Item) ->
                  meta:quote(AFun(Item))
          end,
    {Fun, [{Type,{AFun,Def}}|Mps]}.
