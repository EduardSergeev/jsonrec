
-include_lib("meta/include/meta.hrl").

-import(jsonrecord2,
        [encode_gen/3, decode_gen/3,
         encode_gen/4, decode_gen/4]).
-meta([encode_gen/3, decode_gen/3,
       encode_gen/4, decode_gen/4]).


-define(encode_gen(Type, Record),
        encode_gen(
          meta:quote(Record),
          meta:reify_type(Type),
          meta:reify())).

-define(decode_gen(Type, Struct),
        decode_gen(
          meta:quote(Struct),
          meta:reify_type(Type),
          meta:reify())).
