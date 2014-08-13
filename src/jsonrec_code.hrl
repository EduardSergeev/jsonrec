
-record(mps,
        {defs = dict:new() :: dict(),
         subs :: dict(),
         n_convs :: dict(),
         types = gb_sets:new()}).
