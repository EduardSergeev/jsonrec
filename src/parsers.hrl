-include_lib("meta/include/meta.hrl").

-import(parsers,
        [return/1, bind/2, fail/1,
         mplus/2,
         get_bin/0,
         lift/1,
         left/2, right/2,
         match/1, matches/1, matches/2,
         guard/1,
         many_acc/2, many/1,
         sep_by_till/3, sep_by_till_acc/4, sep_by_till_fold/5,
         skip_many/1, skip_many1/1,
         count/2,
         option/2,

         to_parser/2,

         sequence/1]).

-define(parser_result(Val), parsers:success(Val) | parsers:failure()).
