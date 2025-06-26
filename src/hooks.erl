%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% hooks: generic Erlang hooks application
%%
%% Copyright (c) 2015-2017 Benoit Chesneau <benoitc@benoitcnetwork.eu>
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

-module('hooks').
-behaviour(gen_statem).


%% API to register hooks manually
-export([reg/3, reg/4, reg/5,
         unreg/3, unreg/4, unreg/5,
         mreg/1,
         munreg/1]).

%% API to register hooks from applications
-export([enable_plugin/1, enable_plugin/2,
         disable_plugin/1]).

-export([
  run/2,
  run_fold/3,
  all/2,
  all_till_ok/2,
  only/2
]).

-export([find/1]).

-export([start_link/0]).

-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).
%% State callbacks
-export([not_ready/3, ready/3]).

-include("hooks.hrl").

-define(find_hook(Name),
  case persistent_term:get({hooks, Name}, no_hook) of
    no_hook -> no_hook;
    Res -> Res
  end).

-type hook() :: {atom(), atom(), non_neg_integer()}
| {atom(), atom(), non_neg_integer(), integer()}.
-type hookname() :: any().
-type hooks() :: [{hookname(), [hook()]}].

-export_type([hook/0, hookname/0, hooks/0]).

%% @doc register a `Module:Fun/Arity' as hook, the function name become the hook name.
-spec reg(Module::atom(), Fun::atom(), Arity::non_neg_integer()) -> ok | {error, term()}.
reg(Module, Fun, Arity) ->
  reg(Fun, Module, Fun, Arity, 0).

%% @doc register `Module:Fun/Arity' for the hook HookName
-spec reg(HookName::hookname(), Module::atom(), Fun::atom(), Arity::non_neg_integer()) -> ok | {error, term()}.
reg(HookName, Module, Fun, Arity) ->
  reg(HookName, Module, Fun, Arity, 0).


%% @doc register `Module:Fun/Arity' for the hook HookName with a priority
%% (default is 0)
-spec reg(HookName::hookname(), Module::atom(), Fun::atom(),
  Arity::non_neg_integer(), Priority::integer()) -> ok | {error, term()}.
reg(HookName, Module, Fun, Arity, Priority) ->
  gen_statem:call(?MODULE, {reg, HookName, {Priority, {Module, Fun, Arity}}}).

%% @doc unregister `Module:Fun/Arity', the function name is the hook
-spec unreg(Module::atom(), Function::atom(), Arity::non_neg_integer()) -> ok.
unreg(Module, Fun, Arity) ->
  unreg(Fun, Module, Fun, Arity).


%% @doc unregister `Module:Fun/Arity' for the hook HookName
-spec unreg(HookName::hookname(), Module::atom(), Fun::atom(), Arity::non_neg_integer()) -> ok.
unreg(HookName, Module, Fun, Arity) ->
  unreg(HookName, Module, Fun, Arity, 0).

%% @doc unregister `Module:Fun/Arity' registered for the hook HookName with a
%% priority
-spec unreg(HookName::hookname(), Module::atom(), Fun::atom(),
  Arity::non_neg_integer(), Priority::integer()) -> ok.
unreg(HookName, Module, Fun, Arity, Priority) ->
  gen_statem:call(?MODULE, {unreg, HookName, {Priority, {Module, Fun, Arity}}}).

%% @doc register multiple hooks
-spec mreg(Hooks::hooks()) -> ok | {error, term()}.
mreg(Hooks) ->
  gen_statem:call(?MODULE, {mreg, Hooks}).

%% @doc disable multiple hooks
-spec munreg(Hooks::hooks()) -> ok.
munreg(Hooks) ->
  gen_statem:call(?MODULE, {munreg, Hooks}).

%% @doc enable a plugin
%% This function will start an application if not started and register hooks
%% from it if none have been registered before. Hooks are loaded from the
%% `hooks' key in the application environnement
-spec enable_plugin(Application::atom()) -> ok | {error, term()}.
enable_plugin(Application) ->
  enable_plugin(Application, []).

%% @doc enable a plugin and load paths if needed
-spec enable_plugin(Application::atom(), Paths::[string()]) -> ok | {error, term()}.
enable_plugin(Application, Paths)
  when is_atom(Application), is_list(Paths) ->
  gen_statem:call(?MODULE, {enable_plugin, Application, Paths}).

%% @doc disable a plugin
-spec disable_plugin(Application::atom()) -> ok.
disable_plugin(Application) ->
  gen_statem:call(?MODULE, {disable_plugin, Application}).

%% @doc run all hooks registered for the HookName.
%% Execution can be interrupted if an hook return the atom `stop'.
-spec run(HookName::hookname(), Args::list()) -> ok.
run(HookName, Args) ->
  case ?find_hook(HookName) of
    no_hook -> ok;
    Hooks -> run1(Hooks, HookName, Args)
  end.

run1([], _HookName, _Args) ->
  ok;
run1([{M, F} | Rest], HookName, Args) ->
  Ret = (catch apply(M, F, Args)),
  case Ret of
    {'EXIT', Reason} ->
      error_logger:error_msg("~p~n error running hook: ~p~n",
        [HookName, Reason]),
      run1(Rest, HookName, Args);
    stop ->
      ok;
    _ ->
      run1(Rest, HookName, Args)
  end.

%% @doc fold over all hooks registered for HookName, and return Acc.
%% interuptiuo can be interupted if the hook return `stop' in thacase the
%% latest Acc will be returned or `{stop, Acc}', in the case the Acc will be
%% returned. In other cases the value returned by the hook will be given to
%% the next function.
-spec run_fold(HookName::hookname(), Args::list(), Acc::any()) -> Acc2::any().
run_fold(HookName, Args, Acc) ->
  case ?find_hook(HookName) of
    no_hook -> Acc;
    Hooks -> run_fold1(Hooks, Args, HookName, Acc)
  end.


run_fold1([], _Args, _HookName, Acc) ->
  Acc;
run_fold1([{M, F} | Rest], Args0, HookName, Acc) ->
  Args = Args0 ++ [Acc],
  Ret = (catch apply(M, F, Args)),
  case Ret of
    {'EXIT', Reason} ->
      error_logger:error_msg("~p~n error running hook: ~p~n",
        [HookName, Reason]),
      run_fold1(Rest, Args0, HookName, Acc);
    stop ->
      Acc;
    {stop, NewAcc} ->
      NewAcc;
    _ ->
      run_fold1(Rest, Args0, HookName, Ret)
  end.


%% @doc execute all hooks for this HookName and return all results
-spec all(HookName::hookname(), Args::list()) -> [any()].
all(HookName, Args) ->
  case ?find_hook(HookName) of
    no_hook -> [];
    Hooks -> all1(Hooks, Args, [])
  end.

all1([{Module, Fun} | Rest], Args, Acc) ->
  Res = apply(Module, Fun, Args),
  all1(Rest, Args, [Res | Acc]);
all1([], _Args, Acc) ->
  lists:reverse(Acc).

%% @doc execute all hooks for the HookName until one return ok or {ok, Val}.
%% Otherwise it will return all other results as error.
-spec all_till_ok(HookName::hookname(), Args::list())
    -> ok | {ok, any()} | {error, term()}.
all_till_ok(HookName, Args) ->
  case ?find_hook(HookName) of
    no_hook -> [];
    Hooks -> all_till_ok1(Hooks, Args, [])
  end.

all_till_ok1([{Module, Fun} | Rest], Args, ErrAcc) ->
  Res = apply(Module, Fun, Args),
  case Res of
    ok -> ok;
    {ok, V} -> {ok, V};
    Error -> all_till_ok1(Rest, Args, [Error | ErrAcc])
  end;
all_till_ok1([], _Args, ErrAcc) ->
  {error, lists:reverse(ErrAcc)}.


%% @doc call the top priority hook for the HookName
-spec only(HookName::hookname(), Args::list()) -> any() | no_hook.
only(HookName, Args) ->
  case ?find_hook(HookName) of
    no_hook -> no_hook;
    [{Module, Fun} |_] -> apply(Module, Fun, Args)
  end.


%% @doc retrieve the lists of registered functions for an hook.
-spec find(HookName::hookname()) -> {ok, [{atom(), atom()}]} | error.
find(HookName) ->
  case ?find_hook(HookName) of
    no_hook -> error;
    Hooks -> {ok, Hooks}
  end.

%% @hidden
start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @hidden
callback_mode() ->
  state_functions.

%% @hidden
init([]) ->
  %% build empty list of hooks for safety
  build_hooks([]),
  
  case application:get_env(hooks, wait_for_proc) of
    {ok, Proc} when is_atom(Proc) ->
      spawn_waiter(self(), Proc),
      {ok, not_ready, []};
    _ ->
      %% Send ready message to trigger initialization after init completes
      self() ! ready,
      {ok, ready, []}
  end.

%% State: not_ready
not_ready({call, From}, Request, _State) ->
  %% Immediately respond 'ok' to the caller
  gen_statem:reply(From, ok),
  %% Postpone the actual work
  {keep_state_and_data, [{next_event, internal, {deferred_call, Request}}]};

not_ready(internal, {deferred_call, _Request}, _State) ->
  %% Postpone deferred calls until ready
  {keep_state_and_data, [postpone]};

not_ready(info, ready, _Data) ->
  %% Transition to ready state and initialize
  do_init_hooks(),
  %% gen_statem will automatically deliver postponed events
  {next_state, ready, []}.

%% State: ready
ready({call, From}, {reg, HookName, {Priority, MFA}}, _State) ->
  case do_reg(HookName, {Priority, MFA}) of
    ok ->
      {keep_state_and_data, [{reply, From, ok}]};
    Error ->
      {keep_state_and_data, [{reply, From, Error}]}
  end;

ready({call, From}, {unreg, HookName, {Priority, MFA}}, _State) ->
  do_unreg(HookName, {Priority, MFA}),
  {keep_state_and_data, [{reply, From, ok}]};

ready({call, From}, {mreg, Hooks}, _State) ->
  case do_mreg(Hooks) of
    ok ->
      {keep_state_and_data, [{reply, From, ok}]};
    Error ->
      {keep_state_and_data, [{reply, From, Error}]}
  end;

ready({call, From}, {munreg, Hooks}, _State) ->
  do_munreg(Hooks),
  {keep_state_and_data, [{reply, From, ok}]};

ready({call, From}, {enable_plugin, Application, Paths}, _State) ->
  case do_enable_plugin(Application, Paths) of
    ok ->
      {keep_state_and_data, [{reply, From, ok}]};
    Error ->
      {keep_state_and_data, [{reply, From, Error}]}
  end;
ready({call, From}, {disable_plugin, Application}, _State) ->
  do_disable_plugin(Application),
  {keep_state_and_data, [{reply, From, ok}]};

ready({call, From}, _Msg, _State) ->
  {keep_state_and_data, [{reply, From, badarg}]};

ready(info, ready, _State) ->
  %% Initialize hooks when ready message is received
  do_init_hooks(),
  keep_state_and_data;

ready(info, _Info, _State) ->
  keep_state_and_data;

ready(internal, {deferred_call, {reg, HookName, {Priority, MFA}}}, _State) ->
  %% Process deferred registration
  do_reg(HookName, {Priority, MFA}),
  keep_state_and_data;

ready(internal, {deferred_call, {unreg, HookName, {Priority, MFA}}}, _State) ->
  %% Process deferred unregistration
  do_unreg(HookName, {Priority, MFA}),
  keep_state_and_data;

ready(internal, {deferred_call, {mreg, Hooks}}, _State) ->
  %% Process deferred multi-registration
  do_mreg(Hooks),
  keep_state_and_data;

ready(internal, {deferred_call, {munreg, Hooks}}, _State) ->
  %% Process deferred multi-unregistration
  do_munreg(Hooks),
  keep_state_and_data;

ready(internal, {deferred_call, {enable_plugin, Application, Paths}}, _State) ->
  %% Process deferred plugin enable
  do_enable_plugin(Application, Paths),
  keep_state_and_data;

ready(internal, {deferred_call, {disable_plugin, Application}}, _State) ->
  %% Process deferred plugin disable
  do_disable_plugin(Application),
  keep_state_and_data.

%% @hidden
handle_event(EventType, EventContent, State, _Data) ->
  %% Log unhandled events for debugging
  error_logger:warning_msg("Unhandled event ~p in state ~p: ~p~n", 
                          [EventType, State, EventContent]),
  keep_state_and_data.

%% @hidden
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% @hidden
terminate(_Reason, _StateName, _State) ->
  %% Clean up all persistent_term entries for hooks
  lists:foreach(fun(Key) ->
    case Key of
      {hooks, _} -> persistent_term:erase(Key);
      _ -> ok
    end
  end, persistent_term:get()),
  ok.


%%% private functions
%%%

do_init_hooks() ->
  %% Enable internal hooks plugin
  ok = do_enable_plugin(hooks, []),
  %% Build all hooks from ETS into persistent_term
  build_hooks(),
  %% Run initialization hooks
  hooks:run(init_hooks, []).

spawn_waiter(Server, Proc) ->
  spawn_link(fun() -> wait_proc_loop(Server, Proc) end).


wait_proc_loop(Server, Proc) ->
  case whereis(Proc) of
    undefined ->
      timer:sleep(10),
      wait_proc_loop(Server, Proc);
    _ ->
      Server ! ready
  end.

do_mreg([]) ->
  ok;
do_mreg([{HookName, Hooks0}| Rest]) ->
  case check_hooks(Hooks0, []) of
    {ok, Hooks} ->
      update_hooks(HookName, Hooks),
      do_mreg(Rest);
    Error ->
      Error
  end.


do_reg(HookName, {_Priority, MFA}=Hook) ->
  case check_mfa(MFA) of
    ok ->
      update_hooks(HookName, [Hook]),
      ok;
    Error ->
      Error
  end.

do_munreg([]) ->
  ok;
do_munreg([{HookName, Hooks0} | Rest]) ->
  Hooks = lists:foldl(fun
                        ({_, _, _}=MFA, Acc) ->
                          [{0, MFA} |Acc];
                        ({_, {_, _, _}}=Hook, Acc) ->
                          [Hook | Acc]
                      end, [], Hooks0),
  remove_hooks(HookName, lists:reverse(Hooks)),
  do_munreg(Rest).

do_unreg(HookName, Hook) ->
  remove_hooks(HookName, [Hook]),
  ok.

update_hooks(HookName, HookFuns) ->
  case ets:lookup(?TAB, {h, HookName}) of
    [] ->
      true = ets:insert(?TAB, {{h, HookName}, HookFuns}),
      %% Update persistent_term for new hook
      Sorted = lists:keysort(1, HookFuns),
      MFs = [{M, F} || {_P, {M, F, _A}} <- Sorted],
      persistent_term:put({hooks, HookName}, MFs);
    [{_, Funs}] ->
      %% Merge hooks, avoiding duplicates
      AllFuns = Funs ++ HookFuns,
      %% Remove duplicates while preserving order
      Funs2 = lists:usort(AllFuns),
      true = ets:insert(?TAB, {{h, HookName}, Funs2}),
      %% Update persistent_term with merged hooks
      Sorted = lists:keysort(1, Funs2),
      MFs = [{M, F} || {_P, {M, F, _A}} <- Sorted],
      persistent_term:put({hooks, HookName}, MFs)
  end.

remove_hooks(HookName, HookFuns) ->
  case ets:lookup(?TAB, {h, HookName}) of
    [] ->
      ok;
    [{_, Funs}] ->
      Funs2 = Funs -- HookFuns,
      case Funs2 of
        [] ->
          ets:delete(?TAB, {h, HookName}),
          %% Also remove from persistent_term when no hooks left
          persistent_term:erase({hooks, HookName});
        _ ->
          ets:insert(?TAB, {{h, HookName}, Funs2}),
          %% Update persistent_term with new hook list
          Sorted = lists:keysort(1, Funs2),
          MFs = [{M, F} || {_P, {M, F, _A}} <- Sorted],
          persistent_term:put({hooks, HookName}, MFs)
      end
  end.


do_enable_plugin(Application, Paths) ->
  ok = add_paths(Paths),
  case ets:lookup(?TAB, {p, Application}) of
    [] -> ok;
    [{_, PluginHooks}] ->
      unregister_plugin_hooks(PluginHooks, Application)
  end,

  case load_plugin(Application, Paths) of
    ok ->
      ok;
    Error ->
      Error
  end.

do_disable_plugin(Application) ->
  %% Don't try to stop the hooks application itself
  case Application of
    hooks -> ok;
    _ ->
      case lists:keyfind(Application, 1, application:loaded_applications()) of
        {Application, _, _} ->
          Exports = Application:module_info(exports),
          case lists:member({stop, 0}, Exports) of
            true -> Application:stop();
            false -> application:stop(Application)
          end;
        false -> ok
      end
  end,

  case ets:lookup(?TAB, {p, Application}) of
    [] -> ok;
    [{_, PluginHooks}] -> unregister_plugin_hooks(PluginHooks, Application)
  end,
  ok.

add_paths([]) ->
  ok;
add_paths([Path | Rest]) ->
  _ = code:add_path(Path),

  EbinDir = filelib:wildcard(filename:join(Path, "ebin")),
  ok = code:add_paths(EbinDir),

  %% rebar2 deps
  DepsEbinDir = filelib:wildcard(filename:join(Path, "deps/*/ebin")),
  ok = code:add_paths(DepsEbinDir),
  %% rebar3 deps
  LibEbinDir = filelib:wildcard(filename:join(Path, "lib/*/ebin")),
  ok = code:add_paths(LibEbinDir),
  add_paths(Rest).


maybe_start_plugin(Application) ->
  case lists:keyfind(Application, 1, application:which_applications()) of
    false ->
      _ = application:load(Application),
      Exports = Application:module_info(exports),
      case check_exported_functions([{start, 0}, {stop, 0}], Exports) of
        ok ->
          Application:start();
        _ ->
          Res = application:ensure_all_started(Application),
          case Res of
            {ok, _} -> ok;
            Error -> Error
          end
      end;
    _Else ->
      already_started
  end.

extract_hooks(Application) ->
  application:get_env(Application, hooks, []).

check_hooks([], Hooks) ->
  {ok, lists:reverse(Hooks)};
check_hooks([{_, _, _}=MFA | Rest], Hooks) ->
  check_hooks([{0, MFA} | Rest], Hooks);
check_hooks([{_Priority, MFA}=Hook | Rest], Hooks) ->
  case check_mfa(MFA) of
    ok -> check_hooks(Rest, [Hook | Hooks]);
    Error -> Error
  end.

check_mfa({Mod, Fun, Arity}) ->
  _ = code:ensure_loaded(Mod),
  case lists:member({Fun, Arity}, Mod:module_info(exports)) of
    true -> ok;
    false -> {error, hooks_not_exported}
  end.


build_hooks() ->
  Hooks = ets:select(?TAB, [{{{h,'$1'},'$2'},[],[{{'$1','$2'}}]}]),
  build_hooks(Hooks).

build_hooks(Hooks) ->
  %% Clean up all existing hook entries in persistent_term first
  lists:foreach(fun(Key) ->
    case Key of
      {hooks, _} -> persistent_term:erase(Key);
      _ -> ok
    end
  end, persistent_term:get()),
  
  %% Bulk update persistent_term for all hooks (used during initialization)
  lists:foreach(
    fun({Name, FunsByPriority}) ->
      %% Sort by priority and extract {M, F} tuples
      Sorted = lists:keysort(1, FunsByPriority),
      Funs = [{M, F} || {_P, {M, F, _A}} <- Sorted],
      persistent_term:put({hooks, Name}, Funs)
    end, Hooks),
  
  %% run build hooks
  hooks:run(build_hooks, [Hooks]),
  ok.


unregister_plugin_hooks({Hooks, _Path}, Application) ->
  lists:foreach(fun({HookName, Specs}) ->
    case ets:lookup(?TAB, {h, HookName}) of
      [] -> ok;
      [{_, OldSpecs}] ->
        ToRem = OldSpecs -- Specs,
        NewSpecs = OldSpecs -- ToRem,
        if
          NewSpecs /= [] ->
            ets:insert(?TAB, {{h, HookName}, NewSpecs});
          true ->
            ets:delete(?TAB, {h, HookName}),
            %% Clean up persistent_term when hook is completely removed
            persistent_term:erase({hooks, HookName})
        end
    end
                end, Hooks),
  ets:delete(?TAB, {p, Application}),
  ok.

load_plugin(Application, Paths) ->
  ok = add_paths(Paths),
  Start = maybe_start_plugin(Application),
  if
    Start =:= ok; Start =:= already_started ->
      AppHooks = extract_hooks(Application),

      lists:foreach(fun({HookName, Specs}) ->
        case check_hooks(Specs, []) of
          {ok, Specs2} ->
            case ets:lookup(?TAB, {h, HookName})  of
              [] ->
                ets:insert(?TAB, {{h, HookName}, Specs2}),
                %% Update persistent_term for new hook
                Sorted = lists:keysort(1, Specs2),
                MFs = [{M, F} || {_P, {M, F, _A}} <- Sorted],
                persistent_term:put({hooks, HookName}, MFs);
              [{_, OldSpecs}] ->
                ToRem = OldSpecs -- Specs2,
                NewSpecs = lists:merge(OldSpecs -- ToRem, Specs2),
                ets:insert(?TAB, {{h, HookName}, NewSpecs}),
                %% Update persistent_term with merged hooks
                Sorted = lists:keysort(1, NewSpecs),
                MFs = [{M, F} || {_P, {M, F, _A}} <- Sorted],
                persistent_term:put({hooks, HookName}, MFs)
            end;
          Error ->
            error_logger:error_msg("~p~n error registering: ~p~n", [HookName, Error]),
            ok
        end
                    end, AppHooks),
      ets:insert(?TAB, {{p, Application}, {AppHooks, Paths}}),
      ok;
    true ->
      error_logger:error_msg("~p~n error loading: ~p~n", [Application, Start]),
      Start
  end.

check_exported_functions([], _Exports) ->
  ok;
check_exported_functions([{F, A} | Rest], Exports) ->
  case lists:member({F, A}, Exports) of
    true -> check_exported_functions(Rest, Exports);
    false -> {not_exported, {F, A}}
  end.
