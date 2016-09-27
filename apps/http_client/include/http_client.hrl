% definition for http mock server
-define(API_ROOT_URL, "localhost").
-define(API_PORT, 5000).
-define(API_ONLINE_ENDPOINT, "/").

-define(TIMEOUT, 32).
-define(EXP, 2).

-define(REQ_PARAM(UserId, OnlineState),
  binary_to_list(
    jsx:encode(
      #{type => <<"ONLINE">>,
        payload => (#{
          userId => list_to_binary(UserId),
          online => OnlineState
        })}
    ))).