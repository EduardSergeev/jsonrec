%%%-------------------------------------------------------------------
%%% @author Eduard Sergeev <eduard.sergeev@gmail.com>
%%% @copyright (C) 2012, Eduard Sergeev
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2012 by Eduard Sergeev
%%%-------------------------------------------------------------------
-module(jsonrec_parsers).

%%-export([integer/1, skip_integer/1]).

-compile(export_all).

-record(input, {bin, pos}).


%% define(MOVE(Inp),
%%        begin
%%            #input{bin = <<_, Rest/binary>>, pos = Pos} = Inp,
%%            Inp#input{bin = Rest, pos = Pos + 1}
%%        end).

%% define(MOVE(Inp, Len),
%%        (Inp#input
%%         {bin = binary:part(Inp#input.bin, Len, size(Inp#input.bin) - Len),
%%          pos = Inp#input.pos + Len})).

-define(UNEXPECTED(Pos, Expected),
        error({unexpected_input,
               {at, Pos},
               {expected, Expected}})).

-define(EOF(Expected),
        error({unexpected_eof,
               {expected, Expected}})).



%% integer(#input{bin = Bin, pos = Pos} = Inp) ->
%%     #input{pos = Pos1} = Inp1 = skip_integer(Inp),
%%     Bin1 = binary:part(Bin, 0, Pos1 - Pos),
%%     Integer = list_to_integer(binary_to_list(Bin1)),
%%     {Integer, Inp1}.

singleton(V) ->
    fun(#input{bin = <<C, Rest/binary>>, pos = Pos})
          when C =:= V->
            {ok, {0, #input{bin = Rest, pos = Pos + 1}}};
       (_) ->
            {error, {expected, <<$0>>}}
    end.

range(From, To) ->
    fun(#input{bin = <<C, Rest/binary>>, pos = Pos})
          when C >= From andalso C =< To ->
            {ok, {C, #input{bin = Rest, pos = Pos + 1}}};
       (_) ->
            {error, {expected, <<$0>>}}
    end.

return(Value) ->
    fun(Inp) ->
            {ok, {Value, Inp}}
    end.

right(Left, Right) ->
    fun(Inp) ->
            case Left(Inp) of
                {ok, {_, Inp1}} ->
                    Right(Inp1);
                {error, _} = E ->
                    E
            end
    end.

either(Left, Right) ->
    fun(Inp) ->
            case Left(Inp) of
                {ok, _} = Ok ->
                    Ok;
                {error, _} ->
                    Right(Inp)
            end
    end.

option(Parser, Default) ->
    fun(Inp) ->
           case Parser(Inp) of
               {ok, _} = Ok ->
                   Ok;
               {error, _} ->
                   {ok, {Default, Inp}}
           end
    end.


many(Parser) ->
    fun(Inp) ->
            many_iter(Parser, Inp, [])
    end.

many_iter(Parser, Inp, Acc) ->                
    case Parser(Inp) of
        {ok, {Value, Inp1}} ->
            many_iter(Parser, Inp1, [Value|Acc]);
        {error, _} ->
            lists:reverse(Acc)
    end.


sign() ->
%%    option(right(singleton($-), return(-1)), 1).
    right(singleton($-), return(-1)).

zero() ->
    right(singleton($0), 0).

integer() ->
    option(sign(), 1)



skip_integer(#input{bin = <<$-, Bin/binary>>, pos = Pos} = Inp) ->
    skip_positive(Inp#input{bin = Bin, pos = Pos + 1});
skip_integer(#input{bin = <<>>}) ->
    ?EOF([digit, minus]);
skip_integer(Inp) ->
    skip_natural(Inp).

skip_natural(#input{bin = <<$0, Bin/binary>>, pos = Pos}) ->
    #input{bin = Bin, pos = Pos + 1};
skip_natural(Inp) ->
    skip_positive(Inp).

skip_positive(#input{bin = <<D, Bin/binary>>, pos = Pos} = Inp)
  when D >= $1 andalso D =< $9 ->
    skip_digits(Inp#input{bin = Bin, pos = Pos + 1});
skip_positive(#input{bin = <<>>}) ->
    ?EOF(digit);
skip_positive(#input{pos = Pos}) ->
    ?UNEXPECTED(Pos, digit).

skip_digits(#input{bin = <<D, Bin/binary>>, pos = Pos})
  when D >= $0 andalso D =< $9 ->  
    skip_digits(#input{bin = Bin, pos = Pos + 1});
skip_digits(Inp) ->
    Inp.


%% integer() ->
%%     p([p ||
%%           S <- optional(sign(), 1),
%%           D <- digit(),
%%           Ds <- many(digit()),
%%           return(S * )])
