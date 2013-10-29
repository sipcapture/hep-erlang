hep-erlang
==========

An Erlang implementation of HEP (Homer Encapsulation Protocol).

**WARNING**

This project is currently in the incubator of
[Pannonia Technologies](http://www.pannonia-technologies.com), so there maybe
a lot of changes in how this software works or it might not even work at all.
There might be terrible inconsistencies in or a complete lack of documentation.
So you are warned to use this software at your own risk!

If you'd like to contribute, don't ask, just do it. That's one reason we are
here on GitHub.

Goals
-----

HEP was designed by the developers of the
[Homer SIP Capture Server](http://www.sipcapture.org) solution. HEP is used to
transport metadata and SIP messages from a SIP proxy or SIP user agent to a
capture server. HEP is not limited to transport SIP messages, but before
version 3 of the protocol, the payload could not be distinguished by the
metadata, so transporting anything else than SIP before Version 3 is rather
impractical when conveyed through the same channel. A few major Open Source
SIP-related projects, like [FreeSWITCH](http://www.freeswitch.org),
[Kamailio](http://www.kamailio.org) and [OpenSIPS](http://www.opensips.org)
integrated HEP natively, so this is why this project exists and we also wanted
to be able to capture and/or send HEP messages from Erlang. HEP can use any
kind of transport channel, but most of the time UDP is used.


Usage
-----

TBD

Protocol Version 1
------------------

All integer fields are stored in network byte order.

| Name                | Length   | Description                                                                         |
|---------------------|----------|-------------------------------------------------------------------------------------|
| Version             |  8 bits  | HEP version, always 1 for this version.                                             |
| Length              |  8 bits  | Length of the HEP header, including Version and Length field and excluding payload. |
| Protocol Family     |  8 bits  | Protocol Family PF_INET, PF_INET6 (2 = IPv4, 10 = IPv6).                            |
| Protocol            |  8 bits  | UDP, TCP, TLS, SCTP, etc..                                                          |
| Source Port         | 16 bits  | Source Port of the captured packet in payload.                                      |
| Destination Port    | 16 bits  | Destination Port of the captured packet in payload.                                 |
| Source Address      | *1       | Source IP of the captured packet in payload.                                        |
| Destination Address | *1       | Destination IP of the captured packet in payload.                                   |
| Payload             | *2       | Payload (usually SIP message)                                                       |

*1) Length is 32 bits for IPv4 and 128 bits for IPv6.

*2) Variable length not defined by metadata, so this protocol is really only well suited for UDP.

Protocol Version 2
------------------

All integer fields are stored in network byte order.

| Name                | Length   | Description                                                                         |
|---------------------|----------|-------------------------------------------------------------------------------------|
| Version             |  8 bits  | HEP version, always 2 for this version.                                             |
| Length              |  8 bits  | Length of the HEP header, including Version and Length field and excluding payload. |
| Protocol Family     |  8 bits  | Protocol Family PF_INET, PF_INET6 (2 = IPv4, 10 = IPv6).                            |
| Protocol            |  8 bits  | UDP, TCP, TLS, SCTP, etc..                                                          |
| Source Port         | 16 bits  | Source Port of the captured packet in payload.                                      |
| Destination Port    | 16 bits  | Destination Port of the captured packet in payload.                                 |
| Source Address      | *1       | Source IP of the captured packet in payload.                                        |
| Destination Address | *1       | Destination IP of the captured packet in payload.                                   |
| Seconds             | 32 bits  | The timestamp in seconds since the Epoch when the included payload was captured.    |
| Microseconds        | 32 bits  | The microseconds part of the timestamp.                                             |
| Capture ID of node  | 16 bits  | A capture ID of the node. XXX: What does this exactly mean???                       |
| *unused*            | 16 bits  | *unused*                                                                            |
| Payload             | *2       | Payload (usually SIP message)                                                       |

*1) Length is 32 bits for IPv4 and 128 bits for IPv6.

*2) Variable length not defined by metadata, so this protocol is really only well suited for UDP.

Protocol Version 3
------------------

Each packet in HEP version 3 starts with a header:

| Name                | Length   | Description                                                                      |
|---------------------|----------|----------------------------------------------------------------------------------|
| Protocol Identifier | 4 bytes  | Always contains "HEP3" for this version.                                         |
| Total Length        | 2 bytes  | The total length of this packet, including Protocol Identifier and Total Length. |
| Chunks              | variable | The payload as chunks, with a length of "Total Length" minus 6.                  |

All data is encapsulated in chunks:

| Name          | Length   | Description                                                       |
|---------------|----------|-------------------------------------------------------------------|
| Vendor ID     | 2 bytes  | Vendor Namespace of this chunk.                                   |
| Chunk ID      | 2 bytes  | Vendor specific chunk id.                                         |
| Chunk Length  | 2 bytes  | The total length of this chunk, including Vendor ID and Chunk ID. |
| Chunk Payload | variable | The payload of the chunk, with a length of "Chunk Length minus 6. |

The following chunk data types are defined:

| Type         | Description                                          |
|--------------|------------------------------------------------------|
| octet-string | Arbitrary octet string ("byte array").               |
| utf8-string  | UTF-8 encoded character sequence.                    |
| uint8        | 8 bit unsigned integer.                              |
| uint16       | 16 bit unsigned integer in network byte order.       |
| uint32       | 32 bit unsigned integer in network byte order.       |
| inet4-addr   | 4 octet IPv4 address, most significant octet first.  |
| inet6-addr   | 16 octet IPv6 address, most significant octet first. |


The following Vendor IDs are assigned:

| Vendor ID | Assigned Vendor                 |
|-----------|---------------------------------|
| 16#0000   | Generic chunk types, see below. |
| 16#0001   | FreeSWITCH                      |
| 16#0002   | Kamailio                        |
| 16#0003   | OpenSIPS                        |
| 16#0004   | Asterisk                        |
| 16#0005   | Homer Project                   |
| 16#0006   | SipXecs                         |

Generic chunk types:

| Chunk ID | Type         | Description                          |
|----------|--------------|--------------------------------------|
| 16#0001  | uint8        | IP protocol family                   |
| 16#0002  | uint8        | IP protocol id                       |
| 16#0003  | inet4-addr   | IPv4 source address                  |
| 16#0004  | inet4-addr   | IPv4 destination address             |
| 16#0005  | inet6-addr   | IPv6 source address                  |
| 16#0006  | inet6-addr   | IPv6 destination address             |
| 16#0007  | uint16       | Protocol source port                 |
| 16#0008  | uint16       | Protocol destination port            |
| 16#0009  | uint32       | Timestamp in seconds since the Epoch |
| 16#000a  | uint32       | Microseconds part of timestamp       |
| 16#000b  | uint8        | Protocol type (see table below)      |
| 16#000c  | uint32       | Capture agent id                     |
| 16#000d  | uint16       | keep alive time in seconds           |
| 16#000e  | octet-string | Authenticate key                     |
| 16#000f  | octet-string | captured packet payload              |

Protocol types:

| Type ID | Description        |
|---------|--------------------|
| 16#00   | reserved           |
| 16#01   | SIP                |
| 16#02   | H.323              |
| 16#03   | SDP                |
| 16#04   | RTP                |
| 16#05   | RTCP               |
| 16#06   | MGCP               |
| 16#07   | MEGACO / H.248     |
| 16#08   | M2UA (SS7/SIGTRAN) |
| 16#09   | M3UA (SS7/SIGTRAN) |
| 16#10   | IAX                |

JSON representation
-------------------

```json
[
    {
        "type": "HEP",
        "version": 1,
        "protocolFamily": 2,
        "protocol": 17,
        "srcIp": "192.168.3.11",
        "srcPort": 5060,
        "dstIp": "192.168.3.190",
        "dstPort": 2048,
        "timestamp": "2013-10-29T15:25:53.567Z",
        "timestampUSecs": 123,
        "captureId": null,
        "vendorChunks": [],
        "payload": {
            "type": "SIP",
            "data": "INVITE sip:100@pantech.intern SIP/2.0\r\n..."
        }
    },
    {
        "type": "HEP",
        "version": 2,
        "protocolFamily": 2,
        "protocol": 17,
        "srcIp": "192.168.3.12",
        "srcPort": 5060,
        "dstIp": "192.168.3.11",
        "dstPort": 5060,
        "timestamp": "2013-10-29T15:25:53:577Z",
        "timestampUSecs": 0,
        "captureId": 241,
        "vendorChunks": [],
        "payload": {
            "type": "SIP",
            "data": "INVITE sip:100@pantech.intern SIP/2.0\r\n..."
        }
    }
]
```

Other Software Supporting HEP
-----------------------------

This is third-party software also supporting HEP.

| Name                                            | HEP Versions Supported | Client | Server |
|-------------------------------------------------|------------------------|--------|--------|
| [captagent](http://code.google.com/p/captagent) | 1, 2, 3                | yes    | no     |
| [FreeSWITCH](http://www.freeswitch.org)         | 1                      | yes    | no     |
| [Kamailio](http://www.kamailio.org)             | 1, 2                   | yes    | yes    |
| [OpenSIPS](http://www.opensips.org)             | 1, 2                   | yes    | yes    |

Contributors
------------

- [Matthias Endler](https://github.com/matthias-endler)

License
-------

This project is licensed under the ISC License. See [LICENSE](LICENSE) for details.

---

Copyright &#169; 2013 Matthias Endler. All rights reserved.
