-module(readme).

-include("../include/jsonrec.hrl").

-compile(export_all).

-type country() :: 'AU' | 'RU' | 'UK' | 'US'.
-record(address,
        {line1 :: string(),
         line2 = "" :: string(),
         line3 = "" :: string(),
         country :: country(),
         zip :: string()}).

-type phone_kind() :: work | home | mobile.
-record(phone,
        {kind = mobile :: phone_kind(),
         number :: string(),
         is_prefered :: boolean()}).

-record(person,
        {id :: integer(),
         first_name :: string(),
         last_name :: string(),
         address = unknown :: #address{},
         phones = [] :: [#phone{}]}).


encode(#person{} = Rec) ->
    ?encode_gen(#person{}, Rec).

decode(Bin) ->
    ?decode_gen(#person{}, Bin).
