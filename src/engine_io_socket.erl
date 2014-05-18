-module(engine_io_socket).

-export([handle/3]).
-export([info/3]).

handle(Msg, Req, State) ->
  io:format("MESSAGE~n~p~n", [Msg]),
  {ok, Req, State}.

info(Info, Req, State) ->
  io:format("INFO~n~p~n", [Info]),
  {ok, Req, State}.
