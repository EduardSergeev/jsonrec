
-include_lib("meta/include/meta.hrl").

-import(jsonrecord2, [encode_gen/4, decode_gen/4]).
-meta([encode_gen/4, decode_gen/4]).


-define(encode_gen(Type, Record, Options),
        encode_gen(Record, Type, ?s(meta:reify()), ?s(Options))).

-define(encode_gen(Type, Record),
        ?encode_gen(Type, Record, [])).


-define(decode_gen(Type, Struct, Options),
        decode_gen(Struct, Type, ?s(meta:reify()), ?s(Options))).

-define(decode_gen(Type, Struct),
        ?decode_gen(Type, Struct, [])).
