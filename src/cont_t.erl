-module(cont_t, [InnerMonad]).

-compile({parse_transform, do}).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).
-export([callCC/1, lift/1]).

%% -spec(return/1 :: (A) -> monad(A)).
%% -spec(fail/1 :: (any()) -> monad(_A)).
%% -spec('>>='/2 :: (monad(A), fun ((A) -> monad(B))) -> monad(B)).

-type inner_monad(A) :: monad(A).
-type cont_t(R, A) :: fun((fun((A) -> inner_monad(R))) -> inner_monad(R)).
-type monad(A) :: cont_t(R::any(), A).

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


-spec callCC(K :: fun((fun((A :: any()) -> cont_t(R,B::any()))) -> cont_t(R,A))) -> cont_t(R,A).
callCC(F) ->
    fun(C) ->
	    F1 = F(fun(A) ->
			   fun(_) -> C(A) end    
		   end),
	    F1(C)
    end.

lift(M) ->
    fun (K) ->
	    InnerMonad:'>>='(M, K)
    end.
