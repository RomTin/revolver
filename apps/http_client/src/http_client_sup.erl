-module(http_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_worker/3]).

%% Supervisor callbacks
-export([init/1]).

%% http_client run type
-define(MODE, run).

%% macros
-define(SERVER, ?MODULE).
-define(MODES, [
    {run, http_client_worker},
    {test, http_client_worker_mock}]).
-define(CLIENT_MODULE(Ex),
  case proplists:is_defined(Ex, ?MODES) of
    true -> proplists:get_value(Ex, ?MODES);
    false -> undefined
  end
).
-define(RESTART, {simple_one_for_one, 1, 500}).
-define(CHILD_MFA(Args), {?CLIENT_MODULE(?MODE), start_link, Args}).
-define(CHILD_SPEC(Args), {http_client, ?CHILD_MFA(Args), transient, 2000, worker, [?CLIENT_MODULE(?MODE)]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker(HandlerPid, UserId, Params) ->
  supervisor:start_child(?SERVER, [?CHILD_SPEC([HandlerPid, UserId, Params])]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { ?RESTART, [?CHILD_SPEC([])]} }.

%%====================================================================
%% Internal functions
%%====================================================================
