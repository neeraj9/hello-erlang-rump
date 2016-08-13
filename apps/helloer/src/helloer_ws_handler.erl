%%%-------------------------------------------------------------------
%%% @author nsharma
%%% @copyright (C) 2016, Neeraj Sharma
%%% @doc
%%%
%%% @end
%%% Copyright (c) 2016, Neeraj Sharma <neeraj.sharma@alumni.iitg.ernet.in>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%%   list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% * Neither the name of helloer nor the names of its
%%%   contributors may be used to endorse or promote products derived from
%%%   this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(helloer_ws_handler).
-author("nsharma").

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

%% API

-export([init/3, handle/2, terminate/3]).
-export([
  websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3
]).

-spec(init({tcp, http}, _Req :: term(), _Opts :: term()) ->
  {upgrade, protocol, cowbow_websocket}).
init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.


-spec(handle(Req :: term(), State :: term()) ->
  {ok, Req :: term(), State :: term()}).
handle(Req, State) ->
  lager:debug("Request not expected: ~p", [Req]),
  {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
  {ok, Req2, State}.

-spec(websocket_init(_TransportName :: term(), Req :: term(),
    _Opts :: term()) ->
  {ok, Req :: term(), undefined_state}).
websocket_init(_TransportName, Req, _Opts) ->
  lager:debug("init websocket"),
  {ok, Req, undefined_state}.

-spec(websocket_handle({text, Msg :: term}, Req :: term(), State :: term()) ->
  {reply,
    {text, _Response :: binary()},
    Req :: term(), State :: term(), hibernate}
  ; (_Any :: term(), Req :: term(), State :: term()) ->
    {reply, {text, Response :: binary()}, Req :: term(), State :: term(),
      hibernate}).
websocket_handle({text, Msg}, Req, State) ->
  lager:debug("Got Data: ~p", [Msg]),
  {reply, {text, jsx:encode([{<<"msg">>, Msg}])}, Req, State, hibernate };
  %%{reply, {text, << "responding to ", Msg/binary >>}, Req, State, hibernate };
websocket_handle(_Any, Req, State) ->
  {reply, {text, << "what?" >>}, Req, State, hibernate }.

-spec(websocket_info({timeout, _Ref :: term(), Msg :: term()}, Req :: term(),
        State :: term()) ->
  {reply, {text, Msg :: term()}, Req :: term(), State :: term()}
  ; (_Info :: term(), Req :: term(), State :: term()) ->
    {ok, Req :: term(), State :: term(), hibernate}).
websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  lager:debug("websocket info"),
  {ok, Req, State, hibernate}.

-spec(websocket_terminate(_Reason :: term(), _Req :: term(),
    _State :: term()) ->
  ok).
websocket_terminate(_Reason, _Req, _State) ->
  ok.

-spec(terminate(_Reason :: term(), _Req :: term(), _State :: term()) ->
  ok).
terminate(_Reason, _Req, _State) ->
  ok.
