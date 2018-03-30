%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(yi_stateful_service).
-behaviour(gen_server).

%% yi_stateful_service: simple TCP server.

-export([start_link/0]).
-export([read/0]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {mr_county_guy = 0 :: non_neg_integer()
               ,listen_socket :: gen_tcp:socket()
               }).

-define(LOG(Fmt, List), io:format(user, Fmt++"\n", List)).

%% API

start_link() ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

read() ->
    gen_server:call(?SERVER, ?FUNCTION_NAME).

accept() ->
    ?LOG("asking for accept", []),
    gen_server:cast(?SERVER, ?FUNCTION_NAME).

%% gen_server API

init([]) ->
    ListenOptions = [binary
                    ,{packet, 0}
                    ,{reuseaddr, true}
                    ,{keepalive, false}
                    ,{active, true}
                    ],
    {ok, ListenSocket} = gen_tcp:listen(4000, ListenOptions),
    ?LOG("~s listening on port 4000", [?SERVER]),
    NewState = #state{listen_socket = ListenSocket},
    ?LOG("State: ~p", [NewState#state.mr_county_guy]),
    accept(),
    {ok, NewState}.

%% sync
handle_call(read, _From, State) ->
    OldValue = State#state.mr_county_guy,
    {reply, OldValue, State};
handle_call(_Request, _From, State) ->
    ?LOG("unhandled call: ~p", [_Request]),
    {stop, not_implemented, {error,not_implemented}, State}.

%% async
handle_cast(accept, State=#state{listen_socket = ListenSocket}) ->
    ?LOG("accepting", []),
    case gen_tcp:accept(ListenSocket, 100) of
        {ok, _Socket} -> ok;
        {error, timeout} -> accept();
        {error, closed} -> accept()
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    ?LOG("unhandled cast: ~p", [_Msg]),
    {stop, {error,not_matched}, State}.

%% messages
handle_info({tcp,Socket,<<"PATCH /reset ",_/binary>>}, State) ->
    NewCounter = 0,
    NewState = State#state{mr_county_guy = NewCounter},
    ?LOG("/reset\t State: ~p", [NewCounter]),
    ok = gen_tcp:send(Socket, data(NewCounter)),
    {noreply, NewState};
handle_info({tcp,Socket,<<"PATCH /take ",_/binary>>}, State=#state{mr_county_guy = OldValue}) ->
    NewState = #state{mr_county_guy = NewCounter}
        = State#state{mr_county_guy = 1 + OldValue},
    ?LOG("/take\t State: ~p", [NewCounter]),
    ok = gen_tcp:send(Socket, data(NewCounter)),
    {noreply, NewState};
handle_info({tcp_closed,_Socket}, State) ->
    accept(),
    {noreply, State};
handle_info(_Info, State) ->
    ?LOG("unhandled info: ~p", [_Info]),
    {stop, {error,not_matched}, State}.

terminate(_Reason, _State) ->
    ?LOG("terminating with ~p: ~p", [_State,_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internals

data(Int) ->
    Payload = ["{\"data\": ", integer_to_binary(Int), "}"],
    [ "HTTP/1.1 200 OK\r\n"
      "Content-Length: ", integer_to_binary(iolist_size(Payload)), "\r\n"
      "Content-Type: application/json\r\n"
      "\r\n"
    , Payload
    ].

%% End of Module.
