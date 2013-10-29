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

-module(hep).

-include("hep.hrl").

%% API

-export([parse/1]).
-export([version/1]).
-export([protocol_family/1]).
-export([protocol/1]).
-export([src_ip/1]).
-export([src_port/1]).
-export([dst_ip/1]).
-export([dst_port/1]).
-export([timestamp/1]).
-export([capture_id/1]).
-export([vendor_chunks/1]).
-export([payload_type/1]).
-export([payload/1]).

-type version() :: 1 | 2 | 3.
-export_type([version/0]).

%% PF_INET = 2, PF_INET6 = 10
-type protocol_family() :: 0..255.
-export_type([protocol_family/0]).

%% UDP = 17
-type protocol() :: 0..255.
-export_type([protocol/0]).

-type vendor_id() :: 0..65535.
-export_type([vendor_id/0]).

-type chunk_id() :: 0..65535.
-export_type([chunk_id/0]).

-type chunk_key() :: {vendor_id(), chunk_id()}.
-export_type([chunk_key/0]).

-type chunk_value() :: binary().
-export_type([chunk_value/0]).

-type vendor_chunk() :: {chunk_key(), chunk_value()}.
-export_type([vendor_chunk/0]).

-type payload_type() :: 0..255.
-export_type([payload_type/0]).

-opaque state() :: #hep{}.
-export_type([state/0]).

%% @doc Convenience function to parse HEP v1, v2 and v3.
%%
%% Convenience function, when you don't know, which version will be sent in a
%% transport channel. This is only practical if you use this when your
%% transport is UDP or when you are absolutely sure, you received the complete
%% message and can also distinguish between messages, because HEP v1 and v2
%% do not send a payload length, so they are not well suited for streaming
%% transports.
-spec parse(binary()) -> {ok, state()} | {error, term(), binary()}.
parse(<<?HEP1:8, _Rest/binary>> = Packet) ->
	hep_v1:parse(Packet);
parse(<<?HEP2:8, _Rest/binary>> = Packet) ->
	hep_v2:parse(Packet);
parse(<<?HEP3, _Rest/binary>> = Packet) ->
	hep_v3:parse(Packet);
parse(Other) ->
	{error, unexpected_packet, Other}.

-spec version(state()) -> version().
version(#hep{version = Version}) ->
	Version.

-spec protocol_family(state()) -> protocol_family().
protocol_family(#hep{protocol_family = ProtocolFamily}) ->
	ProtocolFamily.

-spec protocol(state()) -> protocol().
protocol(#hep{protocol = Protocol}) ->
	Protocol.

-spec src_ip(state()) -> inet:ip_address().
src_ip(#hep{src_ip = SrcIp}) ->
	SrcIp.

-spec src_port(state()) -> inet:port_number().
src_port(#hep{src_port = SrcPort}) ->
	SrcPort.

-spec dst_ip(state()) -> inet:ip_address().
dst_ip(#hep{dst_ip = DstIp}) ->
	DstIp.

-spec dst_port(state()) -> inet:port_number().
dst_port(#hep{dst_port = DstPort}) ->
	DstPort.

-spec timestamp(state()) -> erlang:timestamp().
timestamp(#hep{timestamp = Timestamp}) ->
	Timestamp.

-spec capture_id(state()) -> non_neg_integer().
capture_id(#hep{capture_id = CaptureId}) ->
	CaptureId.

-spec vendor_chunks(state()) -> [vendor_chunk()] | 'undefined'.
vendor_chunks(#hep{version = 1}) ->
	undefined;
vendor_chunks(#hep{version = 2}) ->
	undefined;
vendor_chunks(#hep{vendor_chunks = VendorChunks}) ->
	VendorChunks.

-spec payload_type(state()) -> payload_type() | 'undefined'.
payload_type(#hep{version = 1}) ->
	undefined;
payload_type(#hep{version = 2}) ->
	undefined;
payload_type(#hep{payload_type = PayloadType}) ->
	PayloadType.

-spec payload(state()) -> binary().
payload(#hep{payload = Payload}) ->
	Payload.
