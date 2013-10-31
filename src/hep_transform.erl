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

-module(hep_transform).

-include("hep.hrl").

%% API

-export([transform/1]).
-export([transform/2]).

-callback vendor_id() -> hep:vendor_id().
-callback namespace() -> binary().
-callback transform(hep:chunk_id(), binary()) -> {ok, binary(), term()} | no_call.

transform(#hep{} = State) ->
	transform([], State).

-spec transform([module()], hep:state()) -> [proplists:proplist()].
transform(TransformModules, #hep{version = Version, chunks = Chunks})
		when is_list(TransformModules) ->
	WithGeneric = case Version >= 3 of
									true ->
										[hep_generic_transform | TransformModules];
									_ ->
										[hep_generic_transform]
								end,
	EmptyAcc = lists:map(fun (Mod) ->
		{Mod:vendor_id(), {Mod, Mod:namespace(), []}}
	end, WithGeneric),
	Namespaces = transform_chunks(Chunks, EmptyAcc),
	[{<<"hep">>, [{<<"version">>, Version} | Namespaces]}].

transform_chunks([], Acc) ->
	lists:map(fun ({_, {_, Namespace, Values}}) ->
		{Namespace, lists:reverse(Values)}
	end, Acc);
transform_chunks([{<<VendorId:16, ChunkId:16>>, Value} | Rest], Acc) ->
	case lists:keyfind(VendorId, 1, Acc) of
		{_, {Mod, Namespace, ModAcc}} ->
			case Mod:transform(ChunkId, Value) of
				{ok, Name, TransformedValue} ->
					NewAcc = lists:keyreplace(VendorId, 1, Acc,
						{VendorId, {Mod, Namespace, [{Name, TransformedValue} | ModAcc]}}),
					transform_chunks(Rest, NewAcc);
				no_call ->
					transform_chunks(Rest, Acc)
			end;
		_ ->
			transform_chunks(Rest, Acc)
	end.
