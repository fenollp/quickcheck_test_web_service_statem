%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(prop_stateful_service).
-behaviour(proper_statem).

%% proper_stateful_service: PropEr testing stateful_service.

-include_lib("proper/include/proper.hrl").
-include("mylib.hrl").

-record(state, {data :: integer()
               }).

-export([reset/0
        ,take/0
        ]).

%% Callbacks for PropEr
-export([command/1
        ,initial_state/0
        ,next_state/3
        ,precondition/2
        ,postcondition/3
        ]).


%% Types

-type state() :: #state{}.
-type call() :: {call, module(), atom(), list()}.
-type result() :: any().


%% Commands

reset() ->
    io:format(user, "\nreset ~p\n", [self()]),
    ?APP:patch(?BASE "/reset").

take() ->
    io:format(user, "\ntake ~p\n", [self()]),
    ?APP:patch(?BASE "/take").


%% Model

-spec command(state()) -> call().
command(_S) ->
    frequency([{1, {call, ?MODULE, reset, []}}
              ,{10, {call, ?MODULE, take, []}}
              ]).

initial_state() ->
    #state{data = 0
          }.

%% _Call: symbolic call to be performed
%% _S: state that the call will be seeing
%% Purpose: whether _Call should be executed
precondition(_S, _Call) ->
    true.

%% Purpose: check correctness of call result
-spec postcondition(state(), call(), result()) -> boolean().
postcondition(_S, {call,?MODULE,reset,[]}, Result) ->
    Result =:= 0;
postcondition(S, {call,?MODULE,take,[]}, Result) ->
    is_integer(Result)
        andalso Result =:= 1 + S#state.data.

%% NOTE: CallReturn may be symbolic
next_state(S=#state{}, CallReturn, _Call) ->
    S#state{data = CallReturn}.

%% Properties

prop_seq_ticket_dispenser() ->
    {ok, _} = application:ensure_all_started(inets),
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
               begin
                   setup(),
                   {_History,_State,Result}=Ran = run_commands(?MODULE, Cmds),
                   cleanup(),
                   ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~p\n", tuple_to_list(Ran))
                            ,aggregate(command_names(Cmds), Result == ok)
                            )
               end
              )).

%% rebar3 as test proper -m prop_stateful_service -p prop_par_ticket_dispenser -n 1
prop_par_ticket_dispenser() ->
    {ok, _} = application:ensure_all_started(inets),
    ?FORALL(Cmds, parallel_commands(?MODULE),
            ?TRAPEXIT(
               begin
                   setup(),
                   {_Sequential,_Parallel,Result}=Ran = run_parallel_commands(?MODULE, Cmds),
                   cleanup(),
                   ?WHENFAIL(io:format("Sequential: ~w\nParallel: ~w\nResult: ~p\n", tuple_to_list(Ran))
                            ,aggregate(command_names(Cmds), Result == ok)
                            )
               end
              )).

%% Internals

setup() ->
    {ok, _} = application:ensure_all_started(?APP).
%% reset().

cleanup() ->
    ok = application:stop(?APP).

%% End of Module
