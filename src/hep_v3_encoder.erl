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

-module(hep_v3_encoder).

-include("hep.hrl").

%% API

-export([encode/1]).

-spec encode(hep:hep()) -> {ok, iolist()} | {error, term()}.
encode(#hep{chunks = Chunks} = Hep) ->
	case encode(protocol_family, Hep, 0, []) of
		{ok, GenericLength, GenericChunks} ->
			case encode_chunks(Chunks, GenericLength, GenericChunks) of
				{ok, AllChunkLength, AllChunks} ->
					Length = 6 + AllChunkLength,
					{ok, [<<"HEP3", Length:16>> | lists:reverse(AllChunks)]};
				{error, packet_too_large} ->
					{error, {packet_too_large, Hep}}
			end;
		Error ->
			Error
	end.

encode(_, Hep, Len, _) when Len + 6 > 65535 ->
	{error, {packet_too_large, Hep}};
encode(protocol_family, #hep{protocol_family = ProtocolFamily}, _, _) when ProtocolFamily =/= 2, ProtocolFamily =/= 10 ->
	{error, {unknown_protocol_family, ProtocolFamily}};

encode(protocol_family, #hep{protocol_family = ProtocolFamily} = Hep, Len, Acc) ->
	ChunkLen = 6 + 1,
	encode(protocol, Hep, Len + ChunkLen, [<<0:16, 1:16, ChunkLen:16, ProtocolFamily:8>> | Acc]);

encode(protocol, #hep{protocol = Protocol} = Hep, Len, Acc) ->
	ChunkLen = 6 + 1,
	encode(src_ip, Hep, Len + ChunkLen, [<<0:16, 2:16, ChunkLen:16, Protocol:8>> | Acc]);

encode(src_ip, #hep{protocol_family = 2, src_ip = {I0, I1, I2, I3}} = Hep, Len, Acc) ->
	ChunkLen = 6 + 4,
	encode(dst_ip, Hep, Len + ChunkLen, [<<0:16, 3:16, ChunkLen:16, I0:8, I1:8, I2:8, I3:8>> | Acc]);
encode(src_ip, #hep{protocol_family = 10, src_ip = {I0, I1, I2, I3, I4, I5, I6, I7}} = Hep, Len, Acc) ->
	ChunkLen = 6 + 16,
	encode(dst_ip, Hep, Len + ChunkLen, [<<0:16, 5:16, ChunkLen:16, I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>> | Acc]);

encode(dst_ip, #hep{protocol_family = 2, dst_ip = {I0, I1, I2, I3}} = Hep, Len, Acc) ->
	ChunkLen = 6 + 4,
	encode(src_port, Hep, Len + ChunkLen, [<<0:16, 4:16, ChunkLen:16, I0:8, I1:8, I2:8, I3:8>> | Acc]);
encode(dst_ip, #hep{protocol_family = 10, src_ip = {I0, I1, I2, I3, I4, I5, I6, I7}} = Hep, Len, Acc) ->
	ChunkLen = 6 + 16,
	encode(src_port, Hep, Len + ChunkLen, [<<0:16, 6:16, ChunkLen:16, I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>> | Acc]);

encode(src_port, #hep{src_port = SrcPort} = Hep, Len, Acc) ->
	ChunkLen = 6 + 2,
	encode(dst_port, Hep, Len + ChunkLen, [<<0:16, 7:16, ChunkLen:16, SrcPort:16>> | Acc]);

encode(dst_port, #hep{dst_port = DstPort} = Hep, Len, Acc) ->
	ChunkLen = 6 + 2,
	encode(timestamp, Hep, Len + ChunkLen, [<<0:16, 8:16, ChunkLen:16, DstPort:16>> | Acc]);

encode(timestamp, #hep{timestamp = Timestamp} = Hep, Len, Acc) ->
	ChunkLen1 = 6 + 4,
	ChunkLen2 = 6 + 4,
	Secs = hep_util:timestamp_secs(Timestamp),
	Micros = hep_util:timestamp_microsecs(Timestamp),
	encode(payload_type, Hep, Len + ChunkLen1 + ChunkLen2, [<<0:16, 9:16, ChunkLen1:16, Secs:32, 0:16, 10:16, ChunkLen2, Micros:32>> | Acc]);

encode(payload_type, #hep{payload_type = PayloadType} = Hep, Len, Acc) ->
	case valid_payload_type(PayloadType) of
		true ->
			ChunkLen = 6 + 1,
			encode(payload, Hep, Len + ChunkLen, [<<0:16, 10:16, ChunkLen:16, PayloadType:8>> | Acc]);
		_ ->
			{error, {invalid_payload_type, PayloadType}}
	end;

encode(payload, #hep{payload = Payload}, Len, Acc) ->
	ChunkLen = 6 + byte_size(Payload),
	{ok, Len + ChunkLen, [<<0:16, 15:16, ChunkLen:16, Payload/binary>> | Acc]}.

encode_chunks([], Len, Acc) ->
	{ok, Len, Acc};
encode_chunks([{{VendorId, ChunkId}, ChunkValue} | Rest], Len, Acc) ->
	ChunkLength = 6 + byte_size(ChunkValue),
	case (ChunkLength + Len + 6) > 65535 of
		false ->
			encode_chunks(Rest, Len + ChunkLength, [<<VendorId:16, ChunkId:16, ChunkLength:16, ChunkValue/binary>> | Acc]);
		true ->
			{error, packet_too_large}
	end.

-spec valid_payload_type(non_neg_integer()) -> boolean().
valid_payload_type(1) -> true;
valid_payload_type(2) -> true;
valid_payload_type(3) -> true;
valid_payload_type(4) -> true;
valid_payload_type(5) -> true;
valid_payload_type(6) -> true;
valid_payload_type(7) -> true;
valid_payload_type(8) -> true;
valid_payload_type(9) -> true;
valid_payload_type(16) -> true;
valid_payload_type(_) -> false.