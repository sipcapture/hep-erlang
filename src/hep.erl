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

-type uint8() :: 0..255.
-export_type([uint8/0]).

-type uint16() :: 0..65535.
-export_type([uint16/0]).

-type uint32() :: 0..4294967295.
-export_type([uint32/0]).

-type version() :: 1 | 2 | 3.
-export_type([version/0]).

-type chunk_value_length() :: 0..65535.
-export_type([chunk_value_length/0]).

-type chunk_value() :: binary().
-export_type([chunk_value/0]).

-type chunk() :: {{vendor_id(), chunk_id()}, {chunk_value_length(), chunk_value()}}.
-export_type([chunk/0]).

-type vendor_id() :: 1..65535.
-export_type([vendor_id/0]).

-type chunk_id() :: uint16().
-export_type([chunk_id/0]).

-opaque state() :: #hep{}.
-export_type([state/0]).