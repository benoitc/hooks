%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% hooks: generic Erlang hooks application
%%
%% Copyright (c) 2015-2025 Benoît Chesneau
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------


-module('hooks_app').
-moduledoc """
Application callback module for the hooks application.

## Overview

This module implements the application behavior for the hooks system.
It manages the lifecycle of the hooks application, starting the
supervisor tree when the application starts.

## Since

1.0.0
""".

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-doc """
Starts the hooks application.

## Parameters

  * `StartType` - The type of start (normal, takeover, or failover)
  * `StartArgs` - Arguments passed to the application

## Returns

  * `{ok, Pid}` - On successful start, where Pid is the supervisor's PID
  * `{error, Reason}` - If the application fails to start

## Since

1.0.0
""".
start(_StartType, _StartArgs) -> 'hooks_sup':start_link().

-doc """
Stops the hooks application.

## Parameters

  * `State` - The application state (ignored)

## Returns

Always returns `ok`.

## Since

1.0.0
""".
stop(_State) -> ok.
