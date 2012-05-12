-module(cont_m).

-compile({parse_transform, do}).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).

-export([sum/1]).

-type cont(R, A) :: fun((C :: fun((A) -> R)) -> R).
-type monad(A) :: cont(_,A).

-include_lib("erlando/include/monad_specs.hrl").


return(A) ->
    fun(R) -> R(A) end.

'>>='(M, K) ->
    fun(C) ->
	    M(fun(A) ->
		      (K(A))(C)
	      end)
    end.

fail(S) ->
    error(S).


sum(0) ->
    return(0);
sum(N) ->
    [cont_m ||
	N1 <- sum(N-1),
	return(N1+N)].
