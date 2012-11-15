
-include("monads.hrl").

-record(input, {bin, pos}).

-import(mparsers,
        [return/1, bind/2,
         mzero/0, mplus/2, either/2,
         many/1, many_fold/3, option/2, sep_by/2, sep1_by/2,
         singleton/1, range/2,
         '>'/2, 'or'/2,
         oneof/1, noneof/1]).
-meta(['>'/2, 'or'/2, oneof/1]).
