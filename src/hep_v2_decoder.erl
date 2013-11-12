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

-module(hep_v2_decoder).

-include("hep.hrl").

%% API

-export([decode/1]).

-spec decode(binary()) -> {ok, hep:state()} | {error, term(), binary()}.
decode(<<2:8, 28:8, 2:8, Protocol:8, SrcPort:16, DstPort:16,
		S0:8, S1:8, S2:8, S3:8, D0:8, D1:8, D2:8, D3:8,
		Secs:32, USecs:32, NodeId:16, _:16, Payload/binary>>) ->
	{ok, #hep{
		version = 2,
		protocol_family = 2,
		protocol = Protocol,
		src_ip = {S0, S1, S2, S3},
		src_port = SrcPort,
		dst_ip = {D0, D1, D2, D3},
		dst_port = DstPort,
		timestamp = to_timestamp(Secs, USecs),
		node_id = NodeId,
		payload_type = 1,
		payload = Payload
	}};
decode(<<2:8, 52:8, 10:8, Protocol:8, SrcPort:16, DstPort:16,
		S0:16, S1:16, S2:16, S3:16, S4:16, S5:16, S6:16, S7:16,
		D0:16, D1:16, D2:16, D3:16, D4:16, D5:16, D6:16, D7:16,
		Secs:32, USecs:32, NodeId:16, _:16, Payload/binary>>) ->
	{ok, #hep{
		version = 2,
		protocol_family = 10,
		protocol = Protocol,
		src_ip = {S0, S1, S2, S3, S4, S5, S6, S7},
		src_port = SrcPort,
		dst_ip = {D0, D1, D2, D3, D4, D5, D6, D7},
		dst_port = DstPort,
		timestamp = to_timestamp(Secs, USecs),
		node_id = NodeId,
		payload_type = 1,
		payload = Payload
	}};
decode(<<Other/binary>>) ->
	{error, invalid_packet, Other}.

%% internal

to_timestamp(Secs, USecs) ->
	Mega = Secs div 1000000,
	S = Secs rem 1000000,
	{Mega, S, USecs}.