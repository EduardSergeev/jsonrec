
-include_lib("meta/include/meta.hrl").

-import(jsonrec, [encode_gen/4, decode_gen/4]).
-meta([encode_gen/4, decode_gen/4]).


-define(encode_gen(Type, Record, Options),
        encode_gen(?r(Record), Type, ?v(meta:reify()), ?v(Options))).

-define(encode_gen(Type, Record),
        ?encode_gen(Type, Record, [])).


-define(decode_gen(Type, Struct, Options),
        decode_gen(?r(Struct), Type, ?v(meta:reify()), ?v(Options))).

-define(decode_gen(Type, Struct),
        ?decode_gen(Type, Struct, [])).
