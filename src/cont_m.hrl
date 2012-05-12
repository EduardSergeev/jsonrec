
-compile({parse_transform, do}).

-import(cont_m,
	[return/1, fail/1, '>>='/2]).

%% -type cont(R, A) :: fun((C :: fun((A) -> R)) -> R).
%% -type monad(A) :: cont(_,A).
