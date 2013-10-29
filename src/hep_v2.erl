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

-module(hep_v2).

-include("hep.hrl").

%% API

-export([parse/1]).

-spec parse(binary()) -> {ok, hep:state()} | {error, term(), binary()}.
parse(<<Version:8, Length:8, ProtocolFamily:8, Protocol:8, SrcPort:16/big,
DstPort:16/big, Rest/binary>> = Packet) when Version =:= ?HEP2 ->
	parse(Rest, #hep{
		version = Version,
		length = Length,
		protocol_family = ProtocolFamily,
		protocol = Protocol,
		src_port = SrcPort,
		dst_port = DstPort,
		unparsed = Packet});
parse(Other) ->
	{error, unexpected_packet, Other}.

%% internal

parse(
		<<S0:8, S1:8, S2:8, S3:8, D0:8, D1:8, D2:8, D3:8, Payload/binary>>,
		#hep{version = 2, length = 16, protocol_family = ?PF_INET} = State) ->
	SrcIp = {S0, S1, S2, S3},
	DstIp = {D0, D1, D2, D3},
	{ok, State#hep{src_ip = SrcIp, dst_ip = DstIp, payload = Payload}};
parse(<<_/binary>>, #hep{version = 2, protocol_family = ?PF_INET, unparsed = Packet}) ->
	{error, corrupt_packet, Packet};
parse(
		<<S0:16/big, S1:16/big, S2:16/big, S3:16/big, S4:16/big, S5:16/big, S6:16/big, S7:16/big,
		D0:16/big, D1:16/big, D2:16/big, D3:16/big, D4:16/big, D5:16/big, D6:16/big, D7:16/big,
		Payload/binary>>, #hep{version = 2, length = 40, protocol_family = ?PF_INET6} = State) ->
	SrcIp = {S0, S1, S2, S3, S4, S5, S6, S7},
	DstIp = {D0, D1, D2, D3, D4, D5, D6, D7},
	{ok, State#hep{src_ip = SrcIp, dst_ip = DstIp, payload = Payload}};
parse(<<_/binary>>, #hep{version = 2, protocol_family = ?PF_INET6, unparsed = Packet}) ->
	{error, corrupt_packet, Packet};
parse(<<_/binary>>, #hep{unparsed = Packet}) ->
	{error, corrupt_packet, Packet}.
