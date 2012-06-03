-module(rec).

-include("../src/meta.hrl").

-compile(export_all).

-import(jsonrecord2, [encode_gen_ms/1, decode_gen_ms/1]).
-meta([encode_gen_ms/1, decode_gen_ms/1, reify_record/1]).

-record(rec1, {id, fi}).
%% -record(rec2, {id, rec1, int, f1, f2, f3}).

t() ->
    meta:reify(#rec1{}).

%%gen3() ->
%%    encode_gen_ms(meta:reify(#rec1{})).

gen4() ->
    jsonrecord2:decode_gen_ms(meta:reify(#rec1{})).

%%-splice(gen3).

-splice({encode_gen_ms,[{rec1,[{record_field,10,{atom,10,id}},
                               {record_field,10,{atom,10,fi}}]}]}).
-splice(gen4).
