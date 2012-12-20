
-include_lib("meta/include/meta.hrl").
-record(input, {bin, pos}).

-import(monad_parser,
        [do/1, return/1,
         right/2, either/2, many/1, option/2, sep_by/2,
         singleton/1, range/2]).

-meta([do/1]).
