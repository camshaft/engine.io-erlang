-record(state, {
  env,
  handler,
  handler_state,
  transports = [
    {<<"polling">>, engine_io_polling},
    {<<"flashsocket">>, engine_io_flashsocket},
    {<<"websocket">>, engine_io_websocket}
  ],
  transport,
  sid,
  binary
}).
