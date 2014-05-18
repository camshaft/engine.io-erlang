-module(engine_io_websocket).

-export([handle/2]).

-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("engine_io.hrl").

handle(Req, State) ->
  cowboy_websocket:upgrade(Req, State#state.env, ?MODULE, State).

websocket_init(_TransportName, Req, State) ->
  self() ! open,
  {ok, Req, State}.

websocket_handle(Msg, Req, State) ->
  engine_io_socket:handle(Msg, Req, State).

websocket_info(Info, Req, State) ->
  engine_io_socket:info(Info, Req, State).

websocket_terminate(_Reason, _Req, _State) ->
  ok.
