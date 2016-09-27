-module(http_client_worker).

-behaviour(gen_server).

%% Application callbacks
-export([start_link/1]).
%% gen_server callback
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("../include/http_client.hrl").

-define(ONLINE_STATUS, [{online, true}, {offline, false}]).

-record(req_data, {handler_pid, user_id, params}).
-record(state, {time, conn_pid, stream_ref, conn_flag = false, timeout = ?EXP, data = #req_data{} }).

%%====================================================================
%% API
%% @doc data structure: {SIGNATURE, term()}
%% @end
%%====================================================================

%% matching child specification
start_link({http_client, {?MODULE, start_link, [HandlerPid, UserId, Params]}, transient, _, worker, [?MODULE]}) ->
  gen_server:start_link(?MODULE, {HandlerPid, UserId, Params}, []).

init({HandlerPid, UserId, OnlineState}) ->
  %% start http connection
  {ok, ConnPid} = gun:open(?API_ROOT_URL, ?API_PORT),
  link(ConnPid),
  process_flag(trap_exit, true),
  InitTime = hms_to_msec(erlang:timestamp()),
  {ok, #state{time = InitTime, conn_pid = ConnPid, data = #req_data{handler_pid = HandlerPid, user_id = UserId, params = OnlineState}}}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  io:format("http: client terminating...~n"),
  ok.

%%--------------------------------------------------------------------

%% do nothing with call/cast
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Info, State) ->
  {noreply, State}.

%%---------------------------------
%% send, destroy operation handling
%%---------------------------------

%% handling send request
handle_info({send_data, _Option}, State = #state{time = InitTime, timeout = Time, conn_pid = ConnPid, data = Data}) ->
  case Time < ?TIMEOUT  of
    true ->
      Now = hms_to_msec(erlang:timestamp()),
      io:format("----------~7B[ms]----------~n", [Now - InitTime]),

      {_, Pid, Uid, Param} = Data,
      case proplists:is_defined(Param, ?ONLINE_STATUS) of
        true ->
          JsonParam = ?REQ_PARAM(Uid, proplists:get_value(Param, ?ONLINE_STATUS)),
          StreamRef = gun:post(ConnPid,  ?API_ONLINE_ENDPOINT, [{<<"content-type">>, "application/json"}], JsonParam),
          io:format("http: sending...\t: ~p~n", [StreamRef]),
          send_back(Pid, {http_valid, "Request accepted"}),
          {noreply, State#state{stream_ref = StreamRef}};
        false ->
          send_back(Pid, {http_invalid, "Invalid online state requested"}),
          {noreply, State}
      end;
    false ->
      %% handling request timeout
      io:format("http: timeout occurred~n"),
      send_back(Data#req_data.handler_pid, {http_fail, "Request timeout"}),
      send_back(self(), {destroy, []}),
      {noreply, State}
  end;

%% handling destroy request
handle_info({destroy, Reason}, _State = #state{conn_pid = ConnPid, data = Data}) ->
  send_back(Data#req_data.handler_pid, {http_shutdown, Reason}),
  %% kill gun worker process
  gun:shutdown(ConnPid),
  {stop, normal, #state{}};

%%---------------------------------
%% Messages from gun worker
%%---------------------------------

%% error handling
handle_info({gun_error, ConnPid, StreamRef, {Reason, Msg}}, State = #state{timeout = Time}) ->
  io:format("http: catch error\t: ~p~n~n", [{StreamRef, Reason, Msg}]),
  gun:flush(ConnPid),
  gun:flush(StreamRef),
  timer:sleep(Time * 1000),

  send_back(self(), {send_data, []}),
  {noreply, State#state{timeout = Time * ?EXP}};

%% handle gun up/down, and received responses (message from gun worker)
handle_info({gun_up, _Pid, http}, State = #state{conn_flag = Flag, data = Data})
  when Flag =:= false ->
  send_back(Data#req_data.handler_pid, {http_active, "Connection established"}),
  send_back(self(), {send_data, []}),
  {noreply, State#state{conn_flag = true}};

handle_info({gun_response, _ConnPid, StreamRef, IsFin, Status, Header}, State = #state{data = Data}) ->
  io:format("http: ~p\t: ~p [~p]~n\t\t\t~p~n~n", [StreamRef, IsFin, Status, Header]),
  case Status of
    200 ->
      send_back(Data#req_data.handler_pid, {http_success, []});
    _ ->
      send_back(Data#req_data.handler_pid, {http_fail, "not success"})
  end,
  {noreply, State};

handle_info({gun_data, _ConnPid, _StreamRef, IsFin, Body}, State = #state{data = Data}) ->
  case jsx:is_json(Body) of
    true ->
      ParsedBody = jsx:decode(Body);
    false ->
      ParsedBody = Body
  end,
  send_back(Data#req_data.handler_pid, {http_received, {IsFin, ParsedBody}}),
  case IsFin of
    fin ->
      send_back(self(), {destroy, []})
  end,
  {noreply, State};
handle_info({'EXIT', _Pid, normal}, State = #state{data = Data}) ->
  send_back(Data#req_data.handler_pid, {http_fail, "connection error"}),
  send_back(self(), {destroy, "no connection"}),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_Old, State, _Extra) ->
  {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

send_back(Dest, {Signature, Data}) ->
  case is_atom(Signature) of
    true ->
      Dest ! {Signature, Data};
    _ ->
      error
  end.

hms_to_msec({H, M, S}) ->
  round(((H*1000000 + M)*1000000 + S)/1000).