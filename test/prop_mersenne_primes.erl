%% -*- coding: utf-8 -*-
-module(prop_mersenne_primes).

-include_lib("proper/include/proper.hrl").

-compile({parse_transform, memoizer}).
-memoize(is_prime/1).


is_prime(N) when is_integer(N), N =< 1 -> false;
is_prime(2) -> true;
is_prime(N) ->
    is_prime(N, 2).

is_prime(N, N) -> true;
is_prime(N, M) ->
    case N rem M of
        0 -> false;
        _ -> is_prime(N, M + 1)
    end.

prop_mersenne_primes() ->
    ?FORALL(Prime, ?SUCHTHAT(N, integer(), is_prime(N))
           ,begin
                %% io:format(user, "\nPrime = ~p ~p\n", [Prime, is_prime(Prime)]),
                MersennePrime = round(math:pow(2,Prime)) - 1,
                %% io:format(user, "\nMersennePrime = ~p ~p\n", [MersennePrime, is_prime(MersennePrime)]),
                is_prime(MersennePrime)
            end
           ).
