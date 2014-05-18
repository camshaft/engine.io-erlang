-module(engine_io).

-export([upgrade/4]).
-import(cowboy_protocol_helper, [next/3, call/3, terminate/2]).

-include("engine_io.hrl").

-define(HANDLE_ERROR(State),
  {error, ExitStatus, HandlerReq} when is_integer(ExitStatus) ->
    next(HandlerReq, State, ExitStatus)
).

-define(UNKNOWN_TRANSPORT, <<"{\"code\": 0, \"message\": \"Transport unknown\"}">>).
-define(BAD_HANDSHAKE_METHOD, <<"{\"code\": 2, \"message\": \"Bad handshake method\"}">>).
-define(NOT_ALLOWED, <<"{\"code\": 4, \"message\": \"Not allowed\"}">>).

upgrade(Req, Env, Handler, HandlerOpts) ->
  case erlang:function_exported(Handler, engine_init, 2) of
    true ->
      try Handler:engine_init(Req, HandlerOpts) of
        {ok, Req2, HandlerState} ->
          transports(Req2, #state{env = Env, handler = Handler, handler_state = HandlerState})
      catch Class:Reason ->
        cowboy_protocol_helper:error_terminate(Req, #state{handler = Handler, handler_state = HandlerOpts},
            Class, Reason, engine_init, 3)
      end;
    false ->
      transports(Req, #state{env = Env, handler = Handler})
  end.

transports(Req, State) ->
  case call(Req, State, transports) of
    no_call ->
      Transports = fast_key:get(engine_transports, State#state.env, State#state.transports),
      State2 = State#state{transports = Transports},
      next(Req, State2, fun verify/2);
    ?HANDLE_ERROR(State);
    {Transports, Req2, HandlerState} ->
      State2 = State#state{handler_state = HandlerState, transports = Transports},
      next(Req2, State2, fun verify/2)
  end.

verify(Req, State = #state{transports = Transports}) ->
  {Transport, Req2} = cowboy_req:qs_val(<<"transport">>, Req, <<"polling">>),
  {Binary, Req3} = supports_binary(Req2),
  case fast_key:get(Transport, Transports) of
    undefined ->
      error(Req3, State, ?UNKNOWN_TRANSPORT);
    Mod ->
      State2 = State#state{transport = Mod, binary = Binary},
      case cowboy_req:qs_val(<<"sid">>, Req3) of
        {undefined, Req4} ->
          next(Req4, State2, fun allowed/2);
        {Sid, Req4} ->
          next(Req4, State2#state{sid = Sid}, fun lookup/2)
      end
  end.

allowed(Req, State) ->
  case call(Req, State, allowed) of
    no_call ->
      next(Req, State, fun handshake/2);
    ?HANDLE_ERROR(State);
    {true, Req2, HandlerState} ->
      next(Req2, State#state{handler_state = HandlerState}, fun handshake/2);
    {false, Req2, HandlerState} ->
      error(Req2, State#state{handler_state = HandlerState}, ?NOT_ALLOWED)
  end.

handshake(Req, State = #state{transport = Transport}) ->
  %% TODO call `session/2` to get the new sid
  case cowboy_req:method(Req) of
    {<<"GET">>, Req2} ->
      Res = Transport:handle(Req2, State),
      %% TODO call `store/3` if needed
      Res;
    _ ->
      error(Req, State, ?BAD_HANDSHAKE_METHOD)
  end.

lookup(Req, State) ->
  next(Req, State, 501).

%% session(Req, State) ->
%%   next(Req, State, 501).

error(Req, State, Body) ->
  {ok, Req2} = cowboy_req:reply(400, [{<<"content-type">>, <<"application/json">>}], Body, Req),
  terminate(Req2, State).

supports_binary(Req) ->
  case cowboy_req:qs_val(<<"b64">>, Req) of
    {undefined, Req2} ->
      {false, Req2};
    {_, Req2} ->
      {true, Req2}
  end.
