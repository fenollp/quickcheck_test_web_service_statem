-module(eqc_stateful_service).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

initial_state() ->
  1.

reset_args(_State) -> [].

reset() ->
  http(get, "http://localhost:4000/reset").

reset_next(_State, _Result, []) ->
  1.

reset_post(_State, [], Result) ->
  eq(Result, "reset").


take_args(_State) -> [].

take() ->
  Txt = http(get, "http://localhost:4000/take"),
  binary_to_integer(Txt).

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
                               {ok, _} = application:ensure_all_started(mylib),
                               reset(),
                               {History,State,Result} = run_commands(?MODULE, Cmds),
                               ok = application:stop(mylib),
                               pretty_commands(?MODULE, Cmds, {History,State,Result},
                                               aggregate(command_names(Cmds),
                                                         Result == ok))
                           end)).

prop_par_ticket_dispenser() ->
    {ok, _} = application:ensure_all_started(inets),
    with_parameter(print_counterexample, false,
                   ?FORALL(Cmds, parallel_commands(?MODULE),
                           begin
                               {ok, _} = application:ensure_all_started(mylib),
                               reset(),
                               {History,State,Result} = run_parallel_commands(?MODULE, Cmds),
                               ok = application:stop(mylib),
                               pretty_commands(?MODULE, Cmds, {History, State, Result},
                                               aggregate(command_names(Cmds),
                                                         Result == ok))
                           end)).

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
