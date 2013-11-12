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

-module(hep_v1_encoder).

-include("hep.hrl").

%% API

-export([encode/1]).

-spec encode(hep:hep()) -> {ok, binary()} | {error, term()}.
encode(#hep{
		protocol_family = 2, protocol = Protocol,
		src_ip = {S0, S1, S2, S3}, src_port = SrcPort,
		dst_ip = {D0, D1, D2, D3}, dst_port = DstPort,
		payload_type = 1, payload = Payload}) ->
	{ok, <<1:8, 16:8, 2:8, Protocol:8, SrcPort:16, DstPort:16,
	S0:8, S1:8, S2:8, S3:8, D0:8, D1:8, D2:8, D3:8,
	Payload/binary>>};
encode(#hep{
		protocol_family = 10, protocol = Protocol,
		src_ip = {S0, S1, S2, S3, S4, S5, S6, S7}, src_port = SrcPort,
		dst_ip = {D0, D1, D2, D3, D4, D5, D6, D7}, dst_port = DstPort,
		payload_type = 1, payload = Payload}) ->
	{ok, <<1:8, 40:8, 10:8, Protocol:8, SrcPort:16, DstPort:16,
	S0:16, S1:16, S2:16, S3:16, S4:16, S5:16, S6:16, S7:16,
	D0:16, D1:16, D2:16, D3:16, D4:16, D5:16, D6:16, D7:16,
	Payload/binary>>};
encode(#hep{protocol_family = ProtocolFamily}) when ProtocolFamily =/= 2, ProtocolFamily =/= 10 ->
	{error, {unknown_protocol_family, ProtocolFamily}};
encode(#hep{payload_type = PayloadType}) when PayloadType =/= 1 ->
	{error, {unsupported_payload_type, PayloadType}}.