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

-type protoco_family() :: inet | inet6.
-export_type([protoco_family/0]).

-type protocol() :: udp | tcp | sctp | tls.
-export_type([protocol/0]).

-type vendor_id() :: 0..65535.
-export_type([vendor_id/0]).

-type chunk_id() :: 0..65535.
-export_type([chunk_id/0]).

-type vendor_chunk() :: {vendor_id(), chunk_id(), binary()}.
-export_type([vendor_chunk/0]).

-opaque state() :: #hep{}.
-export_type([state/0]).

-spec parse(binary()) -> {ok, state()} | {error, term(), binary()}.
parse(<<1:8, Rest/binary>> = Packet) ->
	parse(Rest, #hep{version = 1, unparsed = Packet});
parse(<<2:8, Rest/binary>> = Packet) ->
	parse(Rest, #hep{version = 2, unparsed = Packet});
parse(<<"HEP3", _Rest/binary>> = Packet) ->
	hep_v3:parse(Packet);
parse(Other) ->
	{error, unexpected_packet, Other}.

%% internal

parse(_, State) ->
	{ok, State}.
