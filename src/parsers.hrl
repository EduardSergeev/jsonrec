-include_lib("meta/include/meta.hrl").

-import(parsers,
        [return/1, bind/2, fail/1,
         get_pos/0, get_bin/0,
         lift/1,
         mplus/2,
         left/2, right/2,
         match/1, matches/1, guard/1,
         fold/3, many_acc/2, many/1,
         skip_many/1, skip_many1/1,
         skip_while/1,
         option/2,

         inst_body/2, to_parser/2]).
