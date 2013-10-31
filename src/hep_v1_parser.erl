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

-module(hep_v1_parser).

-include("hep.hrl").

%% API

-export([parse/1]).

%% @doc parses a complete HEPv1 packet into a HEPv3-like Erlang structure.
%% It also adds the timestamp fields, because in HEPv1 none is transmitted.
%% This is not terribly accurate, but better than nothing, and you can always,
%% ignore the timestamp, by simply checking the version.
-spec parse(binary()) -> {ok, hep:state()} | {error, term(), binary()}.
parse(<<1:8, 16:8, 2:8, Protocol:8, SrcPort:16, DstPort:16, SrcIp:32, DstIp:32,
		Payload/binary>>) ->
	{Secs, USecs} = timestamp(),
	{ok, #hep{
		version = 1,
		chunks = [
			{?HEP_GENERIC(?HEP_IP_PROTOCOL_FAMILY), <<2:8>>},
			{?HEP_GENERIC(?HEP_IP_PROTOCOL), <<Protocol:8>>},
			{?HEP_GENERIC(?HEP_IP_V4_SRC_ADDRESS), <<SrcIp:32>>},
			{?HEP_GENERIC(?HEP_IP_V4_DST_ADDRESS), <<DstIp:32>>},
			{?HEP_GENERIC(?HEP_PROTOCOL_SRC_PORT), <<SrcPort:16>>},
			{?HEP_GENERIC(?HEP_PROTOCOL_DST_PORT), <<DstPort:16>>},
			{?HEP_GENERIC(?HEP_TIMESTAMP_SECS), <<Secs:32>>},
			{?HEP_GENERIC(?HEP_TIMESTAMP_USECS), <<USecs:32>>},
			{?HEP_GENERIC(?HEP_PAYLOAD_TYPE), <<1:8>>},
			{?HEP_GENERIC(?HEP_PAYLOAD), Payload}
		]
	}};
parse(<<1:8, 40:8, 10:8, Protocol:8, SrcPort:16, DstPort:16, SrcIp:128,
		DstIp:128, Payload/binary>>) ->
	{Secs, USecs} = timestamp(),
	{ok, #hep{
		version = 1,
		chunks = [
			{?HEP_GENERIC(?HEP_IP_PROTOCOL_FAMILY), <<2:8>>},
			{?HEP_GENERIC(?HEP_IP_PROTOCOL), <<Protocol:8>>},
			{?HEP_GENERIC(?HEP_IP_V6_SRC_ADDRESS), <<SrcIp:128>>},
			{?HEP_GENERIC(?HEP_IP_V6_DST_ADDRESS), <<DstIp:128>>},
			{?HEP_GENERIC(?HEP_PROTOCOL_SRC_PORT), <<SrcPort:16>>},
			{?HEP_GENERIC(?HEP_PROTOCOL_DST_PORT), <<DstPort:16>>},
			{?HEP_GENERIC(?HEP_TIMESTAMP_SECS), <<Secs:32>>},
			{?HEP_GENERIC(?HEP_TIMESTAMP_USECS), <<USecs:32>>},
			{?HEP_GENERIC(?HEP_PAYLOAD_TYPE), <<1:8>>},
			{?HEP_GENERIC(?HEP_PAYLOAD), Payload}
		]
	}};
parse(Other) ->
	{error, invalid_packet, Other}.

%% internal

timestamp() ->
	{Mega, S, USecs} = os:timestamp(),
	Secs = (Mega * 1000000) + S,
	{Secs, USecs}.
