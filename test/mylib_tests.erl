%% Copyright © 2018 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(mylib_tests).

%% Tests to play with Concuerror.
-include_lib("eunit/include/eunit.hrl").
-include("mylib.hrl").

play_test() ->
    %% {ok,_} = application:ensure_all_started(?APP),
    {ok,Pid} = ?STATEFUL_SERVICE:start_link(),
    1 = ?APP:patch(?BASE "/take"),
    0 = ?APP:patch(?BASE "/reset"),
    1 = ?APP:patch(?BASE "/take"),
    2 = ?APP:patch(?BASE "/take"),
    %% ok = application:stop(?APP).
    ok = gen_server:stop(Pid).
