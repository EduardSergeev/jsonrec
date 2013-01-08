
-include_lib("meta/include/meta.hrl").

-import(jsonrec_encode, [encode_gen/4, encode_gen_encoder/4]).
-import(jsonrec_decode, [decode_gen/4, decode_gen_parser/4]).
-meta([encode_gen/4, encode_gen_encoder/4,
       decode_gen/4, decode_gen_parser/4]).


-define(encode_gen(Type, Record, Options),
        encode_gen(?r(Record), Type, ?v(meta:reify()), ?v(Options))).

-define(encode_gen(Type, Record),
        ?encode_gen(Type, Record, [])).

-define(encode_gen_encoder(Type, Record, Options),
        encode_gen_encoder(?r(Record), Type, ?v(meta:reify()), ?v(Options))).

-define(encode_gen_encoder(Type, Record),
        ?encode_gen_encoder(Type, Record, [])).


-define(decode_gen(Type, Struct, Options),
        decode_gen(?r(Struct), Type, ?v(meta:reify()), ?v(Options))).

-define(decode_gen(Type, Struct),
        ?decode_gen(Type, Struct, [])).

-define(decode_gen_parser(Type, Struct, Options),
        decode_gen_parser(?r(Struct), Type, ?v(meta:reify()), ?v(Options))).

-define(decode_gen_parser(Type, Struct),
        ?decode_gen_parser(Type, Struct, [])).
