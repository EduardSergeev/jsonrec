-module(cont_m).

-compile({parse_transform, do}).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).
-export([callCC/1]).

-type cont(R, A) :: fun((C :: fun((A) -> R)) -> R).
-type monad(A) :: cont(R::any(),A).

-include_lib("erlando/include/monad_specs.hrl").


return(A) ->
    fun(R) -> R(A) end.

'>>='(M, K) ->
    fun(C) ->
	    M(fun(A) ->
		      M2 = K(A),
		      M2(C)
	      end)
    end.

fail(S) ->
    error(S).


-spec callCC(K :: fun((fun((A :: any()) -> cont(R,B::any()))) -> cont(R,A))) -> cont(R,A).
callCC(F) ->
    fun(C) ->
	    F1 = F(fun(A) ->
			   fun(_) -> C(A) end    
		   end),
	    F1(C)
    end.
