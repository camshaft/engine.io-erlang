-module(engine_io_parser).

-export([encode/2]).
-export([decode/1]).
-export([encode_packet/2]).
-export([decode_packet/1]).

-define(EXTRACT(Len, Data, Acc), begin
  L = binary_to_integer(Len),
  case Data of
    <<Message:L/binary, Rest/binary>> ->
      case decode_packet(Message) of
        {ok, Packet} ->
          decode(Rest, [Packet|Acc]);
        Error ->
          Error
      end;
    _ ->
      {error, <<"invalid frame">>}
  end
end).

encode([], _) ->
  {ok, <<"0:">>};
encode(Packets, SupportsBinary) ->
  encode(Packets, SupportsBinary, []).

encode([], _, Acc) ->
  {ok, lists:reverse(Acc)};
encode([Packet|Packets], SupportsBinary, Acc) ->
  case encode_packet(Packet, SupportsBinary) of
    {ok, Message} ->
      encode(Packets, SupportsBinary, [frame_message(Message)|Acc]);
    Error ->
      Error
  end.

decode(<<>>) ->
  {error, <<"invalid frame">>};
decode(<<"0:">>) ->
  {ok, []};
decode(Data) ->
  decode(Data, []).

decode(<<>>, Acc) ->
  {ok, lists:reverse(Acc)};
decode(<<Len:1/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc);
decode(<<Len:2/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc);
decode(<<Len:3/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc);
decode(<<Len:4/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc);
decode(<<Len:5/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc);
decode(<<Len:6/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc);
decode(<<Len:7/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc);
decode(<<Len:8/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc);
decode(<<Len:9/binary, ":", Data/binary>>, Acc) ->
  ?EXTRACT(Len, Data, Acc).

encode_packet({Type, undefined}, _) ->
  {ok, type_to_id(Type)};
encode_packet({Type, Data}, false) ->
  {ok, [<<"b">>, base64:encode(<<(type_to_id(Type))/binary, Data/binary>>)]};
encode_packet({Type, Data}, _) ->
  {ok, [type_to_id(Type), Data]}.

decode_packet(<<"b", Data/binary>>) ->
  decode_packet(base64:decode(Data));
decode_packet(ID) when byte_size(ID) =:= 1 ->
  case id_to_type(ID) of
    {error, _} = Error ->
      Error;
    Type ->
      {ok, {Type, undefined}}
  end;
decode_packet(<<ID:1/binary, Data/binary>>) ->
  case id_to_type(ID) of
    {error, _} = Error ->
      Error;
    Type ->
      {ok, {Type, Data}}
  end.

%% private.

type_to_id(open) -> <<"0">>;
type_to_id(close) -> <<"1">>;
type_to_id(ping) -> <<"2">>;
type_to_id(pong) -> <<"3">>;
type_to_id(message) -> <<"4">>;
type_to_id(upgrade) -> <<"5">>;
type_to_id(noop) -> <<"6">>.

id_to_type(<<"0">>) -> open;
id_to_type(<<"1">>) -> close;
id_to_type(<<"2">>) -> ping;
id_to_type(<<"3">>) -> pong;
id_to_type(<<"4">>) -> message;
id_to_type(<<"5">>) -> upgrade;
id_to_type(<<"6">>) -> noop;
id_to_type(_) -> {error, <<"invalid packet">>}.

frame_message(Message) ->
  [integer_to_binary(iolist_size(Message)), <<":">> ,Message].
