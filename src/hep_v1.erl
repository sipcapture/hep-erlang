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

-module(hep_v1).

-include("hep.hrl").

%% API

-export([new/7]).
-export([parse/1]).
-export([to_packet/1]).

-spec new(hep:protocol_family(), hep:protocol(), inet:ip_address(),
		inet:port_number(), inet:ip_address(), inet:port_number(), binary()) ->
	{ok, hep:state()}.
new(2, Protocol, {S0, S1, S2, S3} = SrcIp, SrcPort, {D0, D1, D2, D3} = DstIp,
		DstPort, Payload) when Protocol >= 0, Protocol =< 255, S0 >= 0, S0 =< 255,
	S1 >= 0, S1 =< 255, S2 >= 0, S2 =< 255, S3 >= 0, S3 =< 255, D0 >= 0,
	D0 =< 255, D1 >= 0, D1 =< 255, D2 >= 0, D2 =< 255, D3 >= 0, D3 =< 255,
	SrcPort >= 0, SrcPort =< 65535, DstPort >= 0, DstPort =< 65535,
	is_binary(Payload) ->
	{ok, #hep{
		version = 1,
		protocol_family = 2,
		protocol = Protocol,
		src_ip = SrcIp,
		src_port = SrcPort,
		dst_ip = DstIp,
		dst_port = DstPort,
		payload = Payload
	}};
new(10, Protocol, {S0, S1, S2, S3, S4, S5, S6, S7} = SrcIp, SrcPort,
		{D0, D1, D2, D3, D4, D5, D6, D7} = DstIp, DstPort, Payload)
	when Protocol >= 0, Protocol =< 255,
	S0 >= 0, S0 =< 65535, S1 >= 0, S1 =< 65535, S2 >= 0, S2 =< 65535, S3 >= 0, S3 =< 65535,
	S4 >= 0, S4 =< 65535, S5 >= 0, S5 =< 65535, S6 >= 0, S6 =< 65535, S7 >= 0, S7 =< 65535,
	D0 >= 0, D0 =< 65535, D1 >= 0, D1 =< 65535, D2 >= 0, D2 =< 65535, D3 >= 0, D3 =< 65535,
	D4 >= 0, D4 =< 65535, D5 >= 0, D5 =< 65535, D6 >= 0, D6 =< 65535, D7 >= 0, D7 =< 65535,
	SrcPort >= 0, SrcPort =< 65535, DstPort >= 0, DstPort =< 65535, is_binary(Payload) ->
	{ok, #hep{
		version = 1,
		protocol_family = 10,
		protocol = Protocol,
		src_ip = SrcIp,
		src_port = SrcPort,
		dst_ip = DstIp,
		dst_port = DstPort,
		payload = Payload
	}}.

-spec parse(binary()) -> {ok, hep:state()} | {error, term(), binary()}.
parse(<<Version:8, Length:8, ProtocolFamily:8, Protocol:8, SrcPort:16/big,
DstPort:16/big, Rest/binary>> = Packet) when Version =:= ?HEP1 ->
	parse(Rest, #hep{
		version = Version,
		length = Length,
		protocol_family = ProtocolFamily,
		protocol = Protocol,
		src_port = SrcPort,
		dst_port = DstPort,
		%% Set timestamp, because this isn't included in HEP v1 messages.
		%% This isn't terribly accurate, but it's better than none at all.
		timestamp = now(),
		unparsed = Packet});
parse(Other) ->
	{error, unexpected_packet, Other}.

-spec to_packet(hep:state()) -> {ok, binary()}.
to_packet(#hep{
		version = 1, protocol_family = ProtocolFamily, protocol = Protocol,
		src_ip = SrcIp, src_port = SrcPort, dst_ip = DstIp, dst_port = DstPort,
		payload = Payload}) ->
	case ProtocolFamily of
		2 ->
			{S0, S1, S2, S3} = SrcIp,
			{D0, D1, D2, D3} = DstIp,
			{ok, <<1:8, 16:8, ProtocolFamily:8, Protocol:8, SrcPort:16/big,
			DstPort:16/big, S0:8, S1:8, S2:8, S3:8, D0:8, D1:8, D2:8, D3:8,
			Payload/binary>>};
		10 ->
			{S0, S1, S2, S3, S4, S5, S6, S7} = SrcIp,
			{D0, D1, D2, D3, D4, D5, D6, D7} = DstIp,
			{ok, <<1:8, 40:8, ProtocolFamily:8, Protocol:8, SrcPort:16/big,
			DstPort:16/big, S0:16/big, S1:16/big, S2:16/big, S3:16/big, S4:16/big,
			S5:16/big, S6:16/big, S7:16/big, D0:16/big, D1:16/big, D2:16/big,
			D3:16/big, D4:16/big, D5:16/big, D6:16/big, D7:16/big, Payload/binary>>}
	end.

%% internal

parse(
		<<S0:8, S1:8, S2:8, S3:8, D0:8, D1:8, D2:8, D3:8, Payload/binary>>,
		#hep{version = 1, length = 16, protocol_family = ?PF_INET} = State) ->
	SrcIp = {S0, S1, S2, S3},
	DstIp = {D0, D1, D2, D3},
	{ok, State#hep{src_ip = SrcIp, dst_ip = DstIp, payload = Payload}};
parse(<<_/binary>>, #hep{version = 1, protocol_family = ?PF_INET, unparsed = Packet}) ->
	{error, corrupt_packet, Packet};
parse(
		<<S0:16/big, S1:16/big, S2:16/big, S3:16/big, S4:16/big, S5:16/big, S6:16/big, S7:16/big,
		D0:16/big, D1:16/big, D2:16/big, D3:16/big, D4:16/big, D5:16/big, D6:16/big, D7:16/big,
		Payload/binary>>, #hep{version = 1, length = 40, protocol_family = ?PF_INET6} = State) ->
	SrcIp = {S0, S1, S2, S3, S4, S5, S6, S7},
	DstIp = {D0, D1, D2, D3, D4, D5, D6, D7},
	{ok, State#hep{src_ip = SrcIp, dst_ip = DstIp, payload = Payload}};
parse(<<_/binary>>, #hep{version = 1, protocol_family = ?PF_INET6, unparsed = Packet}) ->
	{error, corrupt_packet, Packet}.
