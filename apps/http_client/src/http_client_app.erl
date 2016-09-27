-module(http_client_app).

-behaviour(application).
%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  http_client_sup:start_link().

stop(_State) ->
  ok.