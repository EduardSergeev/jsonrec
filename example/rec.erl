-module(rec).

-include("../src/meta.hrl").

-compile(export_all).

-record(rec1, {id, fi}).
-record(rec2, {id, rec1, int}).


t() ->
    meta:reify(#rec1{}).

gen1() ->
    jsonrecord2:encode_gen(meta:reify(#rec1{})).

gen2() ->
    jsonrecord2:encode_gen(meta:reify(#rec2{})).

%%gg() ->
%%    meta:splice(gen()).

-splice(gen1).
%%-splice({{jsonrecord2,encode_gen},[{rec2,[{record_field,8,{atom,8,id}},
%%                                         {record_field,8,{atom,8,rec1}},
%%                                         {record_field,8,{atom,8,int}}]}]}).

%%-splice({{jsonrecord2,encode_gen},[meta:reify(#rec2{})]}).


%%-splice({{jsonrecord2,encode_gen},[rec2]}).

%%ee() ->
%%    {ff, meta:splice(gen())}.

ee() ->
    meta:reify(#rec2{}).
    
