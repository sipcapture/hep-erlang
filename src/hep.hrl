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

-ifndef(HEP_HRL).

-record(hep, {
	version :: 1 | 2 | 3,
	protocol_family :: hep:protoco_family(),
	protocol :: hep:protocol(),
	src_ip :: inet:ip4_address() | inet:ip6_address(),
	src_port :: inet:port_number(),
	dst_ip :: inet:ip4_address() | inet:ip6_address(),
	dst_port :: inet:port_number(),
	timestamp :: erlang:timestamp(),
	capture_id :: non_neg_integer() | 'undefined',
	vendor_chunks = [] :: [hep:vendor_chunk()],
	payload = <<>> :: binary(),

	%% internal state
	unparsed :: binary()
}).

-define(HEP_HRL, true).
-endif.
