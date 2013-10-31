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

%% @private
-module(hep_generic_transform).

-behaviour(hep_transform).

%% API

-export([vendor_id/0]).
-export([namespace/0]).
-export([transform/2]).

vendor_id() -> 16#0000.

namespace() -> <<"generic">>.

transform(16#0001, <<Value:8>>) ->
	{ok, <<"ipProtocolFamily">>, Value};
transform(16#0002, <<Value:8>>) ->
	{ok, <<"ipProtocol">>, Value};
transform(16#0003, <<I0:8, I1:8, I2:8, I3:8>>) ->
	{ok, <<"srcIpAddress">>, {I0, I1, I2, I3}};
transform(16#0004, <<I0:8, I1:8, I2:8, I3:8>>) ->
	{ok, <<"dstIpAddress">>, {I0, I1, I2, I3}};
transform(16#0005, <<I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>) ->
	{ok, <<"srcIpAddress">>, {I0, I1, I2, I3, I4, I5, I6, I7}};
transform(16#0006, <<I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>) ->
	{ok, <<"dstIpAddress">>, {I0, I1, I2, I3, I4, I5, I6, I7}};
transform(16#0007, <<Value:16>>) ->
	{ok, <<"srcPort">>, Value};
transform(16#0008, <<Value:16>>) ->
	{ok, <<"dstPort">>, Value};
transform(16#0009, <<Value:32>>) ->
	{ok, <<"timestampSeconds">>, Value};
transform(16#000a, <<Value:32>>) ->
	{ok, <<"timestampMicroseconds">>, Value};
transform(16#000b, <<Value:8>>) ->
	{ok, <<"payloadType">>, Value};
transform(16#000c, <<Value:32>>) ->
	{ok, <<"captureAgentId">>, Value};
transform(16#000d, <<Value:16>>) ->
	{ok, <<"keepAliveSeconds">>, Value};
transform(16#000e, Value) ->
	{ok, <<"authenticateKey">>, Value};
transform(16#000f, Value) ->
	{ok, <<"payload">>, Value};
transform(_, _) -> no_call.
