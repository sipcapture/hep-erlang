%% Copyright (c) 2013, Matthias Endler <matthias.endler@pantech.at>
%% 
%% Permission to use, copy, modify, and distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(hep_v3).

-define(IP_PROTOCOL_FAMILY, 16#0001).
-define(IP_PROTOCOL_ID, 16#0002).
-define(IP_V4_SRC_ADDRESS, 16#0003).
-define(IP_V4_DST_ADDRESS, 16#0004).
-define(IP_V6_SRC_ADDRESS, 16#0005).
-define(IP_V6_DST_ADDRESS, 16#0006).
-define(PROTOCOL_SRC_PORT, 16#0007).
-define(PROTOCOL_DST_PORT, 16#0008).
-define(TIMESTAMP_SECONDS, 16#0009).
-define(TIMESTAMP_MICROSECONDS, 16#000a).
-define(PROTOCOL_TYPE, 16#000b).
-define(CAPTURE_AGENT_ID, 16#000c).
%% TODO not yet implemented.
%% KEEP_ALIVE_TIME 16#000d
%% AUTHENTICATE_KEY 16#000e
-define(CAPTURE_PACKET_PAYLOAD, 16#000f).

-include("hep.hrl").

%% API

-export([parse/1]).

-spec parse(binary()) -> {ok, hep:state()} | {error, term(), binary()}.
parse(<<?HEP3, Length:16/big, Rest/binary>> = Packet) ->
	parse_chunks(Rest, Length - 6, #hep{version = 3, length = Length, unparsed = Packet});
parse(Other) ->
	{error, unexpected_packet, Other}.

%% internal

parse_chunks(<<ChunkVendor:16/big, ChunkId:16/big, ChunkLength:16/big, Rest/binary>>, Length, State) when ChunkLength >= 6 ->
	ChunkValueLength = ChunkLength - 6,
	<<ChunkValue:ChunkValueLength/binary, Rest2/binary>> = Rest,
	case parse_chunk_value(ChunkVendor, ChunkId, ChunkValueLength, ChunkValue, State) of
		{ok, NewState} ->
			parse_chunks(Rest2, Length - ChunkLength, NewState);
		{error, Type, Value} ->
			{error, Type, Value}
	end.

parse_chunk_value(0, ?IP_PROTOCOL_FAMILY, 1, ChunkValue, #hep{} = State) ->
	<<Value:8>> = ChunkValue,
	{ok, State#hep{protocol_family = Value}};
parse_chunk_value(0, ?IP_PROTOCOL_FAMILY = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?IP_PROTOCOL_ID, 1, ChunkValue, #hep{} = State) ->
	<<Value:8>> = ChunkValue,
	{ok, State#hep{protocol = Value}};
parse_chunk_value(0, ?IP_PROTOCOL_ID = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?IP_V4_SRC_ADDRESS, 4, ChunkValue, #hep{} = State) ->
	<<I0:8, I1:8, I2:8, I3:8>> = ChunkValue,
	Value = {I0, I1, I2, I3},
	{ok, State#hep{src_ip = Value}};
parse_chunk_value(0, ?IP_V4_SRC_ADDRESS = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?IP_V4_DST_ADDRESS, 4, ChunkValue, #hep{} = State) ->
	<<I0:8, I1:8, I2:8, I3:8>> = ChunkValue,
	Value = {I0, I1, I2, I3},
	{ok, State#hep{dst_ip = Value}};
parse_chunk_value(0, ?IP_V4_DST_ADDRESS = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?IP_V6_SRC_ADDRESS, 16, ChunkValue, #hep{} = State) ->
	<<I0:16/big, I1:16/big, I2:16/big, I3:16/big, I4:16/big, I5:16/big, I6:16/big, I7:16/big>> = ChunkValue,
	Value = {I0, I1, I2, I3, I4, I5, I6, I7},
	{ok, State#hep{src_ip = Value}};
parse_chunk_value(0, ?IP_V6_SRC_ADDRESS = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?IP_V6_DST_ADDRESS, 16, ChunkValue, #hep{} = State) ->
	<<I0:16/big, I1:16/big, I2:16/big, I3:16/big, I4:16/big, I5:16/big, I6:16/big, I7:16/big>> = ChunkValue,
	Value = {I0, I1, I2, I3, I4, I5, I6, I7},
	{ok, State#hep{dst_ip = Value}};
parse_chunk_value(0, ?IP_V6_DST_ADDRESS = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?PROTOCOL_SRC_PORT, 2, ChunkValue, #hep{} = State) ->
	<<Value:16/big>> = ChunkValue,
	{ok, State#hep{src_port = Value}};
parse_chunk_value(0, ?PROTOCOL_SRC_PORT = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?PROTOCOL_DST_PORT, 2, ChunkValue, #hep{} = State) ->
	<<Value:16/big>> = ChunkValue,
	{ok, State#hep{dst_port = Value}};
parse_chunk_value(0, ?PROTOCOL_DST_PORT = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?TIMESTAMP_SECONDS, 4, ChunkValue, #hep{timestamp = Timestamp} = State) ->
	<<Value:32/big>> = ChunkValue,
	MegaSecs = Value div 1000000,
	Secs = Value rem 1000000,
	NewTimestamp = case Timestamp of
									 undefined ->
										 {MegaSecs, Secs, 0};
									 {_, _, USecs} ->
										 {MegaSecs, Secs, USecs}
								 end,
	{ok, State#hep{timestamp = NewTimestamp}};
parse_chunk_value(0, ?TIMESTAMP_SECONDS = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?TIMESTAMP_MICROSECONDS, 4, ChunkValue, #hep{timestamp = Timestamp} = State) ->
	<<Value:32>> = ChunkValue,
	NewTimestamp = case Timestamp of
									 undefined ->
										 {0, 0, Value};
									 {MegaSecs, Secs, _} ->
										 {MegaSecs, Secs, Value}
								 end,
	{ok, State#hep{timestamp = NewTimestamp}};
parse_chunk_value(0, ?TIMESTAMP_MICROSECONDS = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?PROTOCOL_TYPE, 1, ChunkValue, #hep{} = State) ->
	<<Value:8>> = ChunkValue,
	{ok, State#hep{payload_type = Value}};
parse_chunk_value(0, ?PROTOCOL_TYPE = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?CAPTURE_AGENT_ID, 4, ChunkValue, #hep{} = State) ->
	<<Value:32/big>> = ChunkValue,
	{ok, State#hep{capture_id = Value}};
parse_chunk_value(0, ?CAPTURE_AGENT_ID = Type, _Length, ChunkValue, #hep{}) ->
	{error, {currupt_chunk, {0, Type}}, ChunkValue};
parse_chunk_value(0, ?CAPTURE_PACKET_PAYLOAD = Type, Length, ChunkValue, #hep{} = State) ->
	case byte_size(ChunkValue) of
		Length ->
			{ok, State#hep{payload = ChunkValue}};
		_ ->
			{error, {currupt_chunk, {0, Type}}, ChunkValue}
	end;
parse_chunk_value(VendorId, ChunkId, Length, ChunkValue, #hep{vendor_chunks = VendorChunks} = State) ->
	case byte_size(ChunkValue) of
		Length ->
			{ok, State#hep{vendor_chunks = [{{VendorId, ChunkId}, ChunkValue} | VendorChunks]}};
		_ ->
			{error, {currupt_chunk, {VendorId, ChunkId}}, ChunkValue}
	end.
