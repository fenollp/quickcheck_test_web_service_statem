-module(eqc_stateful_service).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-inlude("mylib.hrl").

-compile(export_all).

initial_state() ->
    1.

reset_args(_State) -> [].

reset() ->
    ?APP:patch(?BASE "/reset").

reset_next(_State, _Result, []) ->
    1.

reset_post(_State, [], Result) ->
    eq(Result, "reset").


take_args(_State) -> [].

take() ->
    ?APP:patch(?BASE "/take").

take_next(State, _Result, []) ->
    State + 1.

take_post(State, [], Result) ->
    eq(Result, State).


weight(_State, take) -> 10;
weight(_State, reset) -> 1.

prop_ticket_dispenser() ->
    {ok, _} = application:ensure_all_started(inets),
    with_parameter(print_counterexample, false,
                   ?FORALL(Cmds, commands(?MODULE),
                           begin
                               {ok, _} = application:ensure_all_started(?APP),
                               reset(),
                               {History,State,Result} = run_commands(?MODULE, Cmds),
                               ok = application:stop(?APP),
                               pretty_commands(?MODULE, Cmds, {History,State,Result},
                                               aggregate(command_names(Cmds),
                                                         Result == ok))
                           end)).

prop_par_ticket_dispenser() ->
    {ok, _} = application:ensure_all_started(inets),
    with_parameter(print_counterexample, false,
                   ?FORALL(Cmds, parallel_commands(?MODULE),
                           begin
                               {ok, _} = application:ensure_all_started(?APP),
                               reset(),
                               {History,State,Result} = run_parallel_commands(?MODULE, Cmds),
                               ok = application:stop(?APP),
                               pretty_commands(?MODULE, Cmds, {History, State, Result},
                                               aggregate(command_names(Cmds),
                                                         Result == ok))
                           end)).
