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

%% PF_INET = 2, PF_INET6 = 10
-type protoco_family() :: 0..255.
-export_type([protoco_family/0]).

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

%% @doc
%% Convenience function, when you don't know, which version will be sent in a
%% transport channel. This is only practical if you use this when your
%% transport is UDP or when you are absolutely sure, you received the complete
%% message, as HEP v1 and v2 are not well suited for streaming transports.
%% @end
-spec parse(binary()) -> {ok, state()} | {error, term(), binary()}.
parse(<<?HEP1:8, _Rest/binary>> = Packet) ->
	hep_v1:parse(Packet);
parse(<<?HEP2:8, _Rest/binary>> = Packet) ->
	hep_v2:parse(Packet);
parse(<<?HEP3, _Rest/binary>> = Packet) ->
	hep_v3:parse(Packet);
parse(Other) ->
	{error, unexpected_packet, Other}.
