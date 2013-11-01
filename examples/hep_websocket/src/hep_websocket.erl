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

-module(hep_websocket).

-behaviour(gen_server).

%% API

-export([start_link/0]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

-record(state, {
	socket :: inet:socket(),
	pids = [] :: [pid()]
}).

init(_Args) ->
	Port = application:get_env(hep_websocket, hep_udp_port, 9060),
	{ok, Socket} = gen_udp:open(Port, [binary, [{active, true}]]),
	{ok, #state{socket = Socket}}.

handle_call({join, Pid}, _From, #state{pids = Pids} = State) ->

	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({udp, _, SrcIp, SrcPort, Packet}, State) ->

	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
