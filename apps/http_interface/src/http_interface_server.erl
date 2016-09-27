-module(http_interface_server).

-behaviour(gen_server).

%% Application callbacks
-export([start_link/0]).
%% gen_server callback
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Info, State) ->
  {noreply, State}.

handle_info({send, {Pid, UserId, OnlineState}}, State) ->
  http_client_sup:start_worker(Pid, UserId, OnlineState),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

code_change(_Old, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
