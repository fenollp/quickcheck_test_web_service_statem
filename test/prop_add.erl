%% -*- coding: utf-8 -*-
-module(prop_add).

-include_lib("proper/include/proper.hrl").

%% rebar3 as test proper -m prop_add -n 999


add(A, B) -> A + B.

prop_add_isa_group() ->
    ?FORALL({X,Y}, {real(),real()}
           ,true
            andalso add(X,Y) =:= add(Y,X)
            andalso add(X,0) =:= X
           ).
