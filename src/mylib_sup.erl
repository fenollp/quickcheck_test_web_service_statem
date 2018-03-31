-module(mylib_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("mylib.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    I = ?STATEFUL_SERVICE,
    ChildSpec =
        {I, {I, 'start_link', []}, 'permanent', 5 * 1000, 'worker', [I]},
    {ok, { {one_for_all, 0, 1}, [ChildSpec]} }.
