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

-module(hep_v3_decoder).

-include("hep.hrl").

%% API

-export([decode/1]).

-spec decode(binary()) -> {ok, hep:state()} | {error, term(), binary()}.
decode(<<"HEP3", Length:16, _/binary>> = Packet) when Length >= 6 ->
	read_chunk_header(Packet, 6, Length, #hep{version = 3}).

%% @private
%% TODO this needs some refactoring... ugly, ugly, ugly
-spec read_chunk_header(binary(), non_neg_integer(), non_neg_integer(), hep:state()) ->
	{ok, hep:state()} | {error, term(), binary()}.
read_chunk_header(Packet, Offset, Length, Hep0) ->
	<<_:Offset/binary, VendorId:16, ChunkId:16, ChunkLen:16, _/binary>> = Packet,
	case Offset + ChunkLen =< Length of
		false ->
			ValueLen = ChunkLen - 6,
			{ok, Value} = read_chunk_value(Packet, Offset + 6, ValueLen),
			case decode_chunk(VendorId, ChunkId, ValueLen, Value, Hep0) of
				{ok, Hep} ->
					case Offset + ChunkLen =:= Length of
						true ->
							#hep{chunks = Chunks} = Hep,
							{ok, Hep#hep{chunks = lists:reverse(Chunks)}};
						false ->
							read_chunk_header(Packet, Offset + ChunkLen, Length, Hep)
					end;
				{error, Reason} ->
					{error, Reason, Packet}
			end;
		true ->
			{error, invalid_packet, Packet}
	end.

%% @private
-spec read_chunk_value(binary(), non_neg_integer(), non_neg_integer()) -> {ok, binary()}.
read_chunk_value(Packet, Offset, Len) ->
	<<_:Offset/binary, Value:Len/binary, _Rest/binary>> = Packet,
	{ok, Value}.

%% @private
-spec decode_chunk(hep:vendor_id(), hep:chunk_id(), hep:chunk_value_length(), binary, hep:state()) ->
	{ok, hep:state()} | {error, term()}.
decode_chunk(0, 1, 1, <<ProtocolFamily:8>>, Hep)
	when ProtocolFamily =:= 2; ProtocolFamily =:= 10 ->
	{ok, Hep#hep{protocol_family = ProtocolFamily}};
decode_chunk(0, 2, 1, <<Protocol:8>>, Hep) ->
	{ok, Hep#hep{protocol = Protocol}};
decode_chunk(0, 3, 4, <<I0:8, I1:8, I2:8, I3:8>>, #hep{protocol_family = 2} = Hep) ->
	{ok, Hep#hep{src_ip = {I0, I1, I2, I3}}};
decode_chunk(0, 4, 4, <<I0:8, I1:8, I2:8, I3:8>>, #hep{protocol_family = 2} = Hep) ->
	{ok, Hep#hep{dst_ip = {I0, I1, I2, I3}}};
decode_chunk(0, 5, 16, <<I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>,
		#hep{protocol_family = 10} = Hep) ->
	{ok, Hep#hep{src_ip = {I0, I1, I2, I3, I4, I5, I6, I7}}};
decode_chunk(0, 6, 16, <<I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>,
		#hep{protocol_family = 10} = Hep) ->
	{ok, Hep#hep{dst_ip = {I0, I1, I2, I3, I4, I5, I6, I7}}};
decode_chunk(0, 7, 2, <<SrcPort:16>>, Hep) ->
	{ok, Hep#hep{src_port = SrcPort}};
decode_chunk(0, 8, 2, <<DstPort:16>>, Hep) ->
	{ok, Hep#hep{dst_port = DstPort}};
decode_chunk(0, 9, 4, <<TimestampSecs:32>>, Hep) ->
	put_ts_secs(TimestampSecs, Hep);
decode_chunk(0, 10, 4, <<TimestampUSecs:32>>, Hep) ->
	put_ts_usecs(TimestampUSecs, Hep);
decode_chunk(0, 11, 1, <<PayloadType:8>>, Hep) ->
	{ok, Hep#hep{payload_type = PayloadType}};
decode_chunk(0, 12, 4, <<NodeId:32>>, Hep) ->
	{ok, Hep#hep{node_id = NodeId}};
decode_chunk(0, 13, 2, <<_KeepAlive:16>>, Hep) ->
	{ok, Hep};
decode_chunk(0, 14, _, <<_AuthKey/binary>>, Hep) ->
	{ok, Hep};
decode_chunk(0, 15, _, <<Payload/binary>>, Hep) ->
	{ok, Hep#hep{payload = Payload}};
decode_chunk(0, ChunkId, Len, Value, _Hep) when ChunkId >= 1, ChunkId =< 15 ->
	{error, {invalid_chunk, 0, ChunkId, Len, Value}};
decode_chunk(0, _, _, _, Hep) ->
	{ok, Hep};
decode_chunk(VendorId, ChunkId, Len, Value, #hep{chunks = Chunks0} = Hep) ->
	Chunks = [{{VendorId, ChunkId}, {Len, Value}} | Chunks0],
	{ok, Hep#hep{chunks = Chunks}}.

%% @private
-spec put_ts_secs(non_neg_integer(), hep:state()) -> {ok, hep:state()}.
put_ts_secs(TimestampSecs, #hep{timestamp = Timestamp} = Hep) ->
	MegaSecs = TimestampSecs div 1000000,
	Secs = TimestampSecs rem 1000000,
	case Timestamp of
		{_, _, Micros} ->
			{ok, Hep#hep{timestamp = {MegaSecs, Secs, Micros}}};
		undefined ->
			{ok, Hep#hep{timestamp = {MegaSecs, Secs, 0}}}
	end.

%% @private
-spec put_ts_usecs(non_neg_integer(), hep:state()) -> {ok, hep:state()}.
put_ts_usecs(TimestampUSecs, #hep{timestamp = undefined} = Hep) ->
	{ok, Hep#hep{timestamp = {0, 0, TimestampUSecs}}};
put_ts_usecs(TimestampUSecs, #hep{timestamp = {M, S, _}} = Hep) ->
	{ok, Hep#hep{timestamp = {M, S, TimestampUSecs}}}.
