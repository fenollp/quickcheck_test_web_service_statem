%% -*- coding: utf-8 -*-
-module(prop_misc).

-include_lib("proper/include/proper.hrl").

-export([some_simple_type/0
        ,some_simple_type_buuuut_more_complex/0

        ,a_sets_generator/0
        ,another_sets_generator/0

        ,t1/0, t2/0, t3/0
        ]).


add(A, B) -> A + B.
%% rebar3 as test proper -m prop_misc -p prop_add_isa_group -n 999
prop_add_isa_group() ->
    ?FORALL({X,Y}, {real(),real()}
        ,begin
             true
                 andalso add(X,Y) =:= add(Y,X)
                 andalso add(X,0) =:= X
         end
        ).


%% rebar3 as test proper
%% proper_gen:sample/1
%% proper_gen:sampleshrink/1
some_simple_type() -> integer().
some_simple_type_buuuut_more_complex() ->
    T = some_simple_type(),
    ?SIZED(Size, proper_types:resize(Size * 8, T)).

a_sets_generator() ->
    ?LET(SomeList, list(some_simple_type_buuuut_more_complex())
        ,sets:from_list(SomeList)
        ).

%% proper:quickcheck(prop_misc:prop_sets_union_is_commutative(), [{start_size,2}, {max_size,500}, {numtests,999}]).
%% rebar3 as test proper -m prop_misc -p prop_sets_union_is_commutative
prop_sets_union_is_commutative() ->
    ?FORALL({SetA,SetB}, {a_sets_generator(),a_sets_generator()}
           ,?WHENFAIL(io:format("\nSetA = ~w\nSetB = ~w\n"
                               ,[sets:to_list(SetA)
                                ,sets:to_list(SetB)
                                ]
                               )
                     ,sets:union(SetA,SetB) =:= sets:union(SetB,SetA)
                     )
           ).

%%%

another_sets_generator() ->
    ?LET(SomeInt, some_simple_type()
        ,sets:from_list([SomeInt])
        ).

prop_sets_union_is_commutative_fails_OK_but_shrink_harder() ->
    ?FORALL(SetA, another_sets_generator()
           ,?FORALL(SetB, another_sets_generator()
                   ,?WHENFAIL(io:format("\nSetA = ~w\nSetB = ~w\n"
                                       ,[sets:to_list(SetA)
                                        ,sets:to_list(SetB)
                                        ]
                                       )
                             ,sets:union(SetA,SetB) =:= sets:union(SetB,SetA)
                             )
                   )
           ).


t1() ->
    proper_gen:sampleshrink(proper_types:shrink_list(vector(5,integer()))).
t2() ->
    proper_gen:sampleshrink(vector(5, integer())).
t3() ->
    proper_gen:sampleshrink(list(integer())).

prop_shrinking() ->
    ?FORALL(SomeTerm, term(), is_boolean(SomeTerm)).
