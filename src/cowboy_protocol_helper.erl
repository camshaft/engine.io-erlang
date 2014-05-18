-module(cowboy_protocol_helper).

%% TODO break this out into a lib?

-export([call/3]).
-export([call/4]).
-export([exported/4]).
-export([next/3]).
-export([respond/3]).
-export([terminate/2]).

-include("engine_io.hrl").

call(Req, State = #state{handler = Handler, handler_state = HandlerState}, Callback) ->
  case erlang:function_exported(Handler, Callback, 2) of
    true ->
      try
        Handler:Callback(Req, HandlerState)
      catch Class:Reason ->
        error_terminate(Req, State, Class, Reason, Callback, 2)
      end;
    false ->
      no_call
  end.

call(Req, State = #state{handler = Handler, handler_state = HandlerState}, Callback, Arg) ->
  case erlang:function_exported(Handler, Callback, 3) of
    true ->
      try
        Handler:Callback(Arg, Req, HandlerState)
      catch Class:Reason ->
        error_terminate(Req, State, Class, Reason, Callback, 3)
      end;
    false ->
      no_call
  end.

exported(_Req, _State = #state{handler = Handler}, Callback, Arity) ->
  erlang:function_exported(Handler, Callback, Arity).

next(Req, State, Next) when is_function(Next) ->
  Next(Req, State);
next(Req, State, StatusCode) when is_integer(StatusCode) ->
  respond(Req, State, StatusCode).

respond(Req, State, StatusCode) ->
  {ok, Req2} = cowboy_req:reply(StatusCode, Req),
  terminate(Req2, State).

terminate(Req, State = #state{env = Env}) ->
  handler_terminate(Req, State),
  {ok, Req, [{result, ok}|Env]}.

error_terminate(Req, State, Class, Reason, Callback, Arity) ->
  error_terminate(Req, State, Class, Reason, Callback, Arity, 500).
error_terminate(Req, State = #state{handler = Handler, handler_state = HandlerState},
		Class, Reason, Callback, Arity, Status) ->
  handler_terminate(Req, State),
  cowboy_req:maybe_reply(Status, Req),
  erlang:Class([
    {reason, Reason},
    {mfa, {Handler, Callback, Arity}},
    {stacktrace, erlang:get_stacktrace()},
    {req, cowboy_req:to_list(Req)},
    {state, HandlerState}
  ]).

handler_terminate(Req, #state{handler = Handler, handler_state = HandlerState}) ->
  case erlang:function_exported(Handler, terminate, 2) of
    true ->
      ok = Handler:terminate(cowboy_req:lock(Req), HandlerState);
    false ->
      ok
  end.
