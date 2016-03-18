%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(mylib_stateful_service).
-behaviour(gen_server).

%% mylib_stateful_service: simple TCP server.

-export([start_link/0]).
-export([ reset/0
        , take/0
        , read/0
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, { mr_county_guy = 0 :: non_neg_integer()
               , socket :: gen_tcp:socket()
               }).

%% API

start_link() ->
    io:format("~s\n", [?MODULE]),
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

reset() ->
    gen_server:call(?SERVER, reset).

take() ->
    gen_server:call(?SERVER, take).

read() ->
    gen_server:call(?SERVER, read).

%% gen_server API

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(4000, [ binary
                                              , {packet, 0}
                                              , {reuseaddr, true}
                                              , {active, true}
                                              ]),
    io:format("~s listening on port 4000\n", [?SERVER]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    State = #state{socket = Socket},
    {ok, State}.

handle_call(read, _From, State) ->
    OldValue = State#state.mr_county_guy,
    {reply, OldValue, State};
handle_call(_Request, _From, State) ->
    io:format("unhandled call: ~p\n", [_Request]),
    {stop, not_implemented, {error,not_implemented}, State}.

handle_cast(_Msg, State) ->
    io:format("unhandled cast: ~p\n", [_Msg]),
    {stop, {error,not_matched}, State}.

%% handle_info(reset, _From, State) ->
%%     OldValue = State#state.mr_county_guy,
%%     NewState = State#state{mr_county_guy = 0},
%%     {reply, OldValue, NewState};
%% handle_call(take, _From, State) ->
%%     OldValue = State#state.mr_county_guy,
%%     NewState = State#state{mr_county_guy = 1 + OldValue},
%%     {reply, OldValue, NewState};
%gen_tcp:send(Socket, Response)
handle_info(_Info, State) ->
    io:format("unhandled info: ~p\n", [_Info]),
    {stop, {error,not_matched}, State}.

terminate(_Reason, _State) ->
    io:format("terminating with ~p: ~p\n", [_State,_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%% Internals

%% End of Module.
