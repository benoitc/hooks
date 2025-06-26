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

-module('hooks_sup').
-moduledoc """
Top-level supervisor for the hooks application.

## Overview

This supervisor manages the hooks gen_statem process and initializes
the ETS table used for hook storage. It uses a one_for_all restart
strategy to ensure system consistency.

## Supervision Tree

    hooks_sup
        |
        +-- hooks (gen_statem)

## Since

1.0.0
""".

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


-include("hooks.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-doc """
Starts the hooks supervisor.

## Returns

  * `{ok, Pid}` - On success, where Pid is the supervisor process ID
  * `{error, Reason}` - If the supervisor fails to start

## Since

1.0.0
""".
start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-doc """
Initializes the supervisor.

## Description

This function:
1. Creates the ETS table for hook storage
2. Defines the child specification for the hooks gen_statem
3. Returns the supervision strategy

## Returns

Returns `{ok, {SupFlags, ChildSpecs}}` where:
- SupFlags uses one_for_all with no restarts allowed
- ChildSpecs contains the hooks gen_statem worker

## Since

1.0.0
""".
init([]) ->
    %% initi table
    _ = init_tabs(),

	Hooks = {hooks,
					 {hooks, start_link, []},
					 permanent, 5000, worker,
					 [hooks]},
	
	{ok, { {one_for_all, 0, 1}, [Hooks]} }.

-doc """
Initializes the ETS table for hook storage.

## Table Properties

- **Type**: ordered_set - Maintains hooks in sorted order by priority
- **Access**: public - Allows the hooks gen_statem to access it
- **Name**: Named table using the ?TAB macro
- **Concurrency**: Optimized for both read and write concurrency

## Returns

The table identifier or crashes if table creation fails.

## Since

3.0.0
""".
init_tabs() ->
  ets:new(?TAB, [ordered_set, public, named_table,
        {read_concurrency, true},
        {write_concurrency, true}]).




