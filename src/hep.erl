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

-export([get_version/1]).
-export([get_chunk/3]).
-export([get_chunks/1]).

-type version() :: 1 | 2 | 3.
-export_type([version/0]).

-type chunk_key() :: binary().
-export_type([chunk_key/0]).

-type chunk_value() :: binary().
-export_type([chunk_value/0]).

-type chunk() :: {chunk_key(), chunk_value()}.
-export_type([chunk/0]).

-type vendor_id() :: 16#0000..16#ffff.
-export_type([vendor_id/0]).

-type chunk_id() :: 16#0000..16#ffff.
-export_type([chunk_id/0]).

-opaque state() :: #hep{}.
-export_type([state/0]).

-spec get_version(state()) -> version().
get_version(#hep{version = Version}) ->
	Version.

-spec get_chunk(vendor_id(), chunk_id(), state()) ->
	{ok, binary()} | not_found.
get_chunk(VendorId, ChunkId, #hep{chunks = Chunks}) ->
	Key = <<VendorId:16, ChunkId:16>>,
  case lists:keyfind(Key, 1, Chunks) of
		{_Key, Value} ->
			{ok, Value};
		_ ->
			not_found
	end.

-spec get_chunks(state()) -> [chunk()].
get_chunks(#hep{chunks = Chunks}) ->
	Chunks.
