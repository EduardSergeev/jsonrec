-module(reddit).

-include("../include/jsonrec.hrl").

-export([decode/1, decode_many/1,
         fetch/0, fetch/1,
         fetch_many/1, fetch_bare/1]).

-compile(nowarn_unused_record).


-record(content,
        {kind :: binary(),
         data :: any()}).

-record(comment,  %% t1
        {author :: binary(),
         body :: binary(),
         body_html :: binary(),
         link_id :: binary(),
         replies :: binary() | #content{},
         ups :: integer(),
         downs :: integer()}).


-record(post, %% t3
        {id :: binary(),
         author :: binary(),
         hidden :: boolean(),
         is_self :: boolean(),
         num_comments :: integer(),
         permalink :: binary(),
         edited :: boolean() | float()}).


-record(listing,
       {before :: binary(),
        'after' :: binary(),
        children = [] :: [#content{}]}).


-jsonrec([{parser,
           [{{record,comment}, comment_parser},
            {{record,content}, content_parser}]},
          {encoder,
           {{record,content}, content_encoder}}]).


decode(Bin) ->
    case content_parser(Bin) of
        {ok, {Val, _}} ->
            {ok, Val};
        {error, _} = E ->
            E
    end.

decode_many(Bin) ->
    ?decode_gen([#content{}], Bin).


content_parser(Bin) ->
     case ?decode_gen_parser(#content{}, Bin) of
         {ok, {#content{kind = Kind, data = DataBin} = Rec, Bin1}} ->
             case data_parser(Kind, DataBin) of
                 {ok, {Data, _}} ->
                     {ok, {Rec#content{data = Data}, Bin1}};
                 {error, _} = Err ->
                     Err
             end;                 
         {error, _} = Err ->
             Err
     end.


data_parser(<<"t1">>, Bin) ->
    ?decode_gen_parser(#comment{}, Bin);
data_parser(<<"t3">>, Bin) ->
    ?decode_gen_parser(#post{}, Bin);
data_parser(<<"Listing">>, Bin) ->
    ?decode_gen_parser(#listing{}, Bin);
data_parser(_Unk, Bin) ->
    json_parsers:skip_json_p(Bin).


fetch() ->
    fetch("http://www.reddit.com/.json").

fetch(Url) ->
    inets:start(),
    Req = {Url, []},
    case httpc:request(get, Req, [], [{body_format, binary}]) of
        {ok, {{_, 200, _} , _, Bin}} ->
            decode(Bin);
        {ok, {{_, Err, _} , _, _}} ->
            {error, Err};
        Error ->
            Error
    end.

fetch_many(Url) ->
    inets:start(),
    Req = {Url, []},
    case httpc:request(get, Req, [], [{body_format, binary}]) of
        {ok, {{_, 200, _} , _, Bin}} ->
            decode_many(Bin);
        {ok, {{_, Err, _} , _, _}} ->
            {error, Err};
        Error ->
            Error
    end.

fetch_bare(Url) ->
    inets:start(),
    Req = {Url, []},
    case httpc:request(get, Req, [], [{body_format, binary}]) of
        {ok, {{_, 200, _} , _, Bin}} ->
            {ok, Bin};
        {ok, {{_, Err, _} , _, _}} ->
            {error, Err};
        Error ->
            Error
    end.
