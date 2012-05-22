-module(cont_m_tests).

-compile({parse_transform, do}).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("proper_utils.hrl").

%% -include("../src/cont_m.hrl").


-compile(export_all).

prop_return() ->
    ?FORALL(Input, any(),
	    begin
		Cont =
		    do([cont_m ||
			   cont_m:return(Input)]),
		Cont(fun id/1) == Input
	    end).

prop_left_identity() ->
    ?FORALL({A, F, FR},
	    {any(),
	     function([any()], cont()),
	     function([any()],any())},
	    begin
		M1 = cont_m:'>>='(cont_m:return(A), F),
		M2 = F(A),
		M1(FR) == M2(FR)
	    end).

prop_right_identity() ->
    ?FORALL({M, FR},
	    {cont(),
	     function([any()],any())},
	    begin
		M1 = cont_m:'>>='(M, fun cont_m:return/1),
		M2 = M,
		M1(FR) == M2(FR)
	    end).

prop_associativity() ->
    ?FORALL({M, F, G, FR},
	    {cont(),
	     function([any()], cont()),
	     function([any()], cont()),
	     function([any()],any())},
	    begin
		M1 = cont_m:'>>='(
                        (cont_m:'>>='(M,F)),
		        G),
		M2 = cont_m:'>>='(
		       M,
		       (fun(X) ->
				cont_m:'>>='(F(X), G)
			end)),
		M1(FR) == M2(FR)
	    end).


prop_fib_equiv() ->
    ?FORALL(N, integer(0, 20),
	   fib(N) == (fib_c(N))(fun id/1)).

fib(N) when N < 2 ->
    N;
fib(N) ->
    fib(N-1) + fib(N-2).

fib_c(N) when N < 2 ->
    cont_m:return(N);
fib_c(N) ->
    do([cont_m ||
	   F1 <- fib_c(N-1),
	   F2 <- fib_c(N-2),
	   cont_m:return(F1+F2)]).

fib_c_s(N) when N < 2 ->
    M = cont_t:new(state_t:new(identity_m)),
    M:return(N);
fib_c_s(N) ->
    S = state_t:new(identity_m),
    M = cont_t:new(S),
    do([M ||
	   C <- M:lift(S:get()),
	   M:lift(S:put(C+1)),
	   F1 <- fib_c_s(N-1),
	   F2 <- fib_c_s(N-2),
	   M:return(F1+F2)]).

%%
%% Generators
%%
cont() ->
    ?LET(A, any(),
	 fun(C) ->
		 C(A)
	 end).

id(A) ->
    A.
