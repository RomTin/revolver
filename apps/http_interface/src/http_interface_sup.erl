-module(http_interface_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(RESTART, {one_for_one, 100, 1000}).
-define(CHILD_MFA, {http_interface_server, start_link, []}).
-define(CHILD_SPEC, {http_interface, ?CHILD_MFA, permanent, 2000, worker, [http_interface_server]}).
%%====================================================================
%% API functions
%%====================================================================

%TODO throttling
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { ?RESTART, [?CHILD_SPEC]} }.

%%====================================================================
%% Internal functions
%%====================================================================
