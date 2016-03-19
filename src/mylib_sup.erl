%%%-------------------------------------------------------------------
%% @doc mylib top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mylib_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    I = yi_stateful_service,
    ChildSpec =
        {I, {I, 'start_link', []}, 'permanent', 5 * 1000, 'worker', [I]},
    {ok, { {one_for_all, 0, 1}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
