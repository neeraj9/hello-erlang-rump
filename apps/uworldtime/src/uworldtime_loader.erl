%%%-------------------------------------------------------------------
%%% @author nsharma
%%% @copyright (C) 2016, Neeraj Sharma
%%% @doc
%%%
%%% @end
%%% Copyright (c) 2016, Neeraj Sharma <neeraj.sharma@alumni.iitg.ernet.in>.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(uworldtime_loader).
-author("nsharma").

%% API
-export([start/0]).

-spec(start() ->
  ok).
start() ->
  application:start(syntax_tools),
  application:start(compiler),
  %% crypto is required by cowboy and must be started
  %% irrespective of whether https is used or not
  application:start(crypto),
  application:start(goldrush),
  application:start(lager),
  application:start(ranch),
  application:start(cowlib),
  application:start(cowboy),
  % for issuing httpc:request(Url)
  application:start(inets),
  % for starting ssl so that httpc can issue https requests
  ssl:start(),
  application:start(uworldtime),
  ok.
