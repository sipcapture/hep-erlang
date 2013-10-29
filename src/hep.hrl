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

-define(PF_INET, 2).
-define(PF_INET6, 10).

-define(HEP1, 1).
-define(HEP2, 2).
-define(HEP3, "HEP3").

-record(hep, {
	version :: hep:version(),
	protocol_family :: hep:protoco_family(),
	protocol :: hep:protocol(),
	src_ip :: inet:ip_address(),
	src_port :: inet:port_number(),
	dst_ip :: inet:ip_address(),
	dst_port :: inet:port_number(),
	timestamp :: erlang:timestamp() | 'undefined',
	capture_id :: non_neg_integer() | 'undefined',

	%% vendor_chunks and payload_type are
	%% undefined in HEP version 1 and 2
	vendor_chunks = [] :: [hep:vendor_chunk()],
	payload_type :: hep:payload_type() | 'undefined',
	payload = <<>> :: binary(),

	%% internal state

	%% HEP v1 and v2 this is only the header length.
	%% HEP v3 this is the total length.
	length :: non_neg_integer(),
	unparsed :: binary()
}).

-define(HEP_HRL, true).
-endif.
