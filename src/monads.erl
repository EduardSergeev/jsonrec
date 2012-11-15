-module(monads).

-include_lib("meta/include/meta.hrl").

-export([do/1]).

-meta([do/1]).
%% -define(re(Syntax), erl_syntax:revert(Syntax)).

do(QLC) ->
    {lc, _Ln, Res, Exprs} = ?e(QLC),
    expand(Exprs, ?v(Res)).

expand([], Res) ->
    ?q(return(?s(Res)));
expand([{generate, _Ln, Pat, Expr} | Rs], Res) ->
    ?q(bind(?s(?v(Expr)),
            fun(?s(?v(Pat))) ->
                    ?s(expand(Rs, Res))
            end));
expand([Expr | Rs], Res) ->
    ?q(bind(?s(?v(Expr)),
            fun(_) ->
                    ?s(expand(Rs, Res))
            end)).
