
-include_lib("meta/include/meta.hrl").

-import(jsonrecord2, [encode_gen/4, decode_gen/4]).
-meta([encode_gen/4, decode_gen/4]).


-define(encode_gen(Type, Record, Options),
        encode_gen(
          meta:quote(Record),
          meta:reify_type(Type),
          meta:reify(),
          Options)).

-define(encode_gen(Type, Record),
        ?encode_gen(Type, Record, [])).


-define(decode_gen(Type, Struct, Options),
        decode_gen(
          meta:quote(Struct),
          meta:reify_type(Type),
          meta:reify(),
          Options)).

-define(decode_gen(Type, Struct),
        ?decode_gen(Type, Struct, [])).
