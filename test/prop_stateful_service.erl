%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(prop_stateful_service).
-behaviour(proper_statem).

%% proper_stateful_service: PropEr testing stateful_service.

-include_lib("proper/include/proper.hrl").

-record(state, { data :: integer()
               }).

-export([ reset/0
        , take/0
        ]).

%% API: proper
-export([ command/1
        , initial_state/0
        , next_state/3
        , precondition/2
        , postcondition/3
        ]).


%% Types

-type state() :: #state{}.
-type call() :: {call, module(), atom(), list()}.
-type result() :: any().


%% Model

-spec command(state()) -> call().
command(_S) ->
    frequency([ {1, {call, ?MODULE, reset, []}}
              , {10, {call, ?MODULE, take, []}}
              ]).

reset() ->
    Txt = http(get, "http://localhost:4000/reset"),
    binary_to_integer(Txt).

take() ->
    Txt = http(get, "http://localhost:4000/take"),
    binary_to_integer(Txt).

initial_state() ->
    #state{ data = 0
          }.

%% _CallReturn is often called V
%%   * must use symbolic calls  on it as it handles symbolic vars
%%   eg: don't call hd(V), instead return:
%%     S#state{users = [{call,erlang,hd,[V]}|S#state.users]}
next_state(S=#state{}, CallReturn, _Call) ->
    S#state{data = CallReturn}.

%% _Call: symbolic call to be performed
%% _S: state that the call will be seeing
%% Purpose: whether _Call should be executed
precondition(_S, _Call) ->
    true.

%% Purpose: check correctness of call result
-spec postcondition(state(), call(), result()) -> boolean().
postcondition(_S, {call,?MODULE,reset,[]}, Result) ->
    Result == 0;
postcondition(S, {call,?MODULE,take,[]}, Result) ->
    Result == 1 + S#state.data.

%% Properties

prop_seq_ticket_dispenser() ->
    {ok, _} = application:ensure_all_started(inets),
    ?FORALL(Cmds, proper_statem:commands(?MODULE),
            ?TRAPEXIT(
               begin
                   setup(),
                   {_History,_State,Result} = Ran = proper_statem:run_commands(?MODULE, Cmds),
                   cleanup(),
                   ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~p\n", tuple_to_list(Ran))
                            ,aggregate(command_names(Cmds), Result == ok)
                            )
               end
              )).

prop_par_ticket_dispenser() ->
    {ok, _} = application:ensure_all_started(inets),
    ?FORALL(Cmds, proper_statem:parallel_commands(?MODULE),
            ?TRAPEXIT(
               begin
                   setup(),
                   {_Sequential,_Parallel,Result} = Ran = proper_statem:run_parallel_commands(?MODULE, Cmds),
                   cleanup(),
                   ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~p\n", tuple_to_list(Ran))
                            ,aggregate(command_names(Cmds), Result == ok)
                            )
               end
              )).

%% Internals

setup() ->
    {ok, _} = application:ensure_all_started(mylib).
    %% reset().

cleanup() ->
    ok = application:stop(mylib).

http(get, URL) ->
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, Txt}} =
        httpc:request(get, {URL, []}, [], []),
    {_, LengthStr} = lists:keyfind("content-length", 1, Headers),
    Length =
        - length("{\"data\": ")
        + list_to_integer(LengthStr)
        - length("}"),
    <<"{\"data\": ", Data:Length/binary, "}">> = list_to_binary(Txt),
    Data.

%% End of Module
