%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% hooks: generic Erlang hooks application
%%
%% Copyright (c) 2015 Benoit Chesneau <benoitc@benoitcnetwork.eu>
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
-behaviour(gen_server).


%% API to register hooks manually
-export([reg/3, reg/4, reg/5,
         unreg/3, unreg/4, unreg/5,
         mreg/1,
         munreg/1]).

%% API to register hooks from applications
-export([enable_plugin/1, enable_plugin/2,
         disable_plugin/1]).

-export([run/2,
         run_fold/3,
         all/2,
         all_till_ok/2,
         only/2]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("syntax_tools/include/merl.hrl").

-define(TAB, ?MODULE).

-record(state, {ready=true,
                on_init,
                on_build}).

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
    gen_server:call(?MODULE, {reg, HookName, {Priority, {Module, Fun, Arity}}}).

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
    gen_server:call(?MODULE, {unreg, HookName, {Priority, {Module, Fun, Arity}}}).

%% @doc register multiple hooks
-spec mreg(Hooks::hooks()) -> ok | {error, term()}.
mreg(Hooks) ->
    gen_server:call(?MODULE, {mreg, Hooks}).

%% @doc disable multiple hooks
-spec munreg(Hooks::hooks()) -> ok.
munreg(Hooks) ->
    gen_server:call(?MODULE, {munreg, Hooks}).

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
    gen_server:call(?MODULE, {enable_plugin, Application, Paths}).

%% @doc disable a plugin
-spec disable_plugin(Application::atom()) -> ok.
disable_plugin(Application) ->
    gen_server:call(?MODULE, {disable_plugin, Application}).

%% @doc run all hooks registered for the HookName.
%% Execution can be interrupted if an hook return the atom `stop'.
-spec run(HookName::hookname(), Args::list()) -> ok.
run(HookName, Args) ->
    case hooks_list:find(HookName) of
        {ok, Hooks} -> run1(Hooks, HookName, Args);
        _ -> ok
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
    case hooks_list:find(HookName) of
        {ok, Hooks} -> run_fold1(Hooks, Args, HookName, Acc);
        _ -> Acc
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
    case hooks_list:find(HookName) of
        {ok, Hooks} -> all1(Hooks, Args, []);
        _ -> []
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
    case hooks_list:find(HookName) of
        {ok, Hooks} -> all_till_ok1(Hooks, Args, []);
        _ -> []
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
-spec only(HookName::hookname(), Args::list()) -> any() | hooks_not_found.
only(HookName, Args) ->
    case hooks_list:find(HookName) of
        {ok, [{Module, Fun} |_]} ->
            apply(Module, Fun, Args);
        Error ->
            Error
    end.

%% @hidden
start_link() ->
    _ = init_tabs(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init_tabs() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [ordered_set, public, named_table,
                           {read_concurrency, true},
                           {write_concurrency, true}]);
        _ ->
            true
    end.

%% @hidden
init([]) ->
    Ready = case application:get_env(hooks, wait_for_proc) of
                {ok, Proc} when is_atom(Proc) ->
                    spawn_waiter(self(), Proc),
                    false;
                _ ->
                    true
            end,

    self() ! init_internal_hooks,

    {ok, #state{ready=Ready}}.

%% @hidden
handle_call({reg, HookName, {Priority, MFA}}, _From, State) ->
    case do_reg(HookName, {Priority, MFA}) of
        ok ->
            maybe_build_hooks(State),
            {reply, ok, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({unreg, HookName, {Priority, MFA}}, _From, State) ->
    do_unreg(HookName, {Priority, MFA}),
    maybe_build_hooks(State),
    {reply, ok, State};

handle_call({mreg, Hooks}, _From, State) ->
    case do_mreg(Hooks) of
        ok ->
            maybe_build_hooks(State),
            {reply, ok, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({munreg, Hooks}, _From, State) ->
    do_munreg(Hooks),
    maybe_build_hooks(State),
    {reply, ok, State};

handle_call({enable_plugin, Application, Paths}, _From, State) ->
    case do_enable_plugin(Application, Paths) of
        ok ->
            maybe_build_hooks(State),
            {reply, ok, State};
        Error ->
            {reply, Error, State}
    end;
handle_call({disable_plugin, Application}, _From, State) ->
    do_disable_plugin(Application),
    maybe_build_hooks(State),
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, badarg, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(ready, State) ->
    build_hooks(),
    {noreply, State#state{ready=true}};

handle_info(init_internal_hooks, State) ->
    ok = do_enable_plugin(hooks, []),
    %% we always add internal plugins
    build_hooks(),
    %% run init hooks
    hooks:run(init_hooks, []),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @hidden
terminate(_Reason, _Srv) ->
    ok.


%%% private functions
%%%
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
            true = ets:insert(?TAB, {{h, HookName}, HookFuns});
        [{_, Funs}] ->
            Funs2 = lists:keysort(1, Funs ++ HookFuns),
            true = ets:insert(?TAB, {{h, HookName}, Funs2})
    end.

remove_hooks(HookName, HookFuns) ->
    case ets:lookup(?TAB, {h, HookName}) of
        [] ->
            ok;
        [{_, Funs}] ->
            Funs2 = Funs -- HookFuns,
            case Funs2 of
                [] ->
                    ets:delete(?TAB, {h, HookName});
                _ ->
                    ets:insert(?TAB, {{h, HookName}, Funs2})
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
    case lists:keyfind(Application, 1, application:loaded_applications()) of
        true ->
            Exports = Application:module_info(exports),
            case lists:member({stop, 0}, Exports) of
                true -> Application:stop();
                false -> application:stop(Application)
            end;
        _ -> ok
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
    case erlang:function_exported(Mod, Fun, Arity) of
        true -> ok;
        false -> {error, hooks_not_exported}
    end.

maybe_build_hooks(#state{ready=false}) -> ok;
maybe_build_hooks(#state{ready=true}) -> build_hooks().

build_hooks() ->
    Hooks = ets:select(?TAB, [{{{h,'$1'},'$2'},[],[{{'$1','$2'}}]}]),
    build_hooks(Hooks).

build_hooks(Hooks) ->
    %% hooks:find_hook/1 function
    Cs0 = lists:foldl(fun({Name, FunsByPriority}, Acc) ->
                              Funs = [{M, F} || {_P, {M, F, _A}} <- FunsByPriority],
                              [?Q(["(_@Name@) ->",
                                   "{ok, _@Funs@}"]) | Acc]
                      end, [], Hooks),
    Cs = lists:reverse([?Q("(_) -> hook_not_found") | Cs0]),
    Find = erl_syntax:function(merl:term('find'), Cs),

    %% build module
    Module = ?Q("-module('hooks_list')."),
    Exported = ?Q("-export(['find'/1])."),
    Functions = [ ?Q("'@_F'() -> [].") || F <- [Find]],
    Forms = lists:flatten([Module, Exported, Functions]),
    merl:compile_and_load(Forms, [verbose]),
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
                                          ets:delete(?TAB, {h, HookName})
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
                                                  ets:insert(?TAB, {{h, HookName}, Specs2});
                                              [{_, OldSpecs}] ->
                                                  ToRem = OldSpecs -- Specs2,
                                                  NewSpecs = lists:merge(OldSpecs -- ToRem, Specs2),
                                                  ets:insert(?TAB, {{h, HookName}, NewSpecs})
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

-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


hook1() ->
	[].

hook1(A, B) ->
	[A | B].


hook2() ->
	[].

hook2(A, B) ->
	[A | B].

hook3(A, B, C) ->
	{hook3, [A, B, C]}.

hook_add(A) ->
    A + A.

hook_add1(A) ->
    A + A.

hook_add2(A) ->
    A + 2 *A.

hooks_ok1() ->
    next.

hooks_ok2() ->
    ok.

hooks_ok3() ->
    next.

hooks_ok4() ->
    {ok, 0}.

basic_test() ->
    {ok, _} = application:ensure_all_started(hooks),

    Hooks = [{a, [{?MODULE, hook1, 0},
                  {?MODULE, hook2, 0}]},
             {b, [{?MODULE, hook1, 2},
                  {?MODULE, hook2, 2}]}],

    lists:foreach(fun({Name, Specs}) ->
                          Res = [hooks:reg(Name, M, F, A) || {M, F, A} <- Specs],
                          ?assertEqual([ok, ok], Res)
                  end, Hooks),

    FoundA = hooks_list:find(a),
    ?assertEqual({ok, [{?MODULE, hook1}, {?MODULE, hook2}]}, FoundA),
    FoundB = hooks_list:find(b),
    ?assertEqual({ok, [{?MODULE, hook1}, {?MODULE, hook2}]}, FoundB),
    ?assertEqual(hook_not_found, hooks_list:find(c)),
    ok = hooks:reg(c, ?MODULE, hook3, 3),
    ?assertEqual({ok, [{?MODULE, hook3}]}, hooks_list:find(c)),
    ?assertEqual(ok, hooks:unreg(c, ?MODULE, hook3, 3)),
    ?assertEqual(hook_not_found, hooks_list:find(c)),
    ?assertEqual({error, hooks_not_exported}, hooks:reg(c, ?MODULE, hook3, 2)),

    lists:foreach(fun({Name, Specs}) ->
                          Res = [hooks:unreg(Name, M, F, A) || {M, F, A} <- Specs],
                          ?assertEqual([ok, ok], Res)
                  end, Hooks),
    ?assertEqual(hook_not_found, hooks_list:find(a)),
    ?assertEqual(hook_not_found, hooks_list:find(b)),
    ok.

mreg_test() ->
    {ok, _} = application:ensure_all_started(hooks),

    Hooks = [{a, [{?MODULE, hook1, 0},
                  {?MODULE, hook2, 0}]},
             {b, [{?MODULE, hook1, 2},
                  {?MODULE, hook2, 2}]}],
    ?assertEqual(ok, hooks:mreg(Hooks)),
    FoundA = hooks_list:find(a),
    ?assertEqual({ok, [{?MODULE, hook1}, {?MODULE, hook2}]}, FoundA),
    FoundB = hooks_list:find(b),
    ?assertEqual({ok, [{?MODULE, hook1}, {?MODULE, hook2}]}, FoundB),
    ?assertEqual(ok, hooks:munreg(Hooks)),
    ?assertEqual(hook_not_found, hooks_list:find(a)),
    ?assertEqual(hook_not_found, hooks_list:find(b)),
    ok.

run_test() ->
    {ok, _} = application:ensure_all_started(hooks),

    Hooks = [{a, [{?MODULE, hook1, 0},
                  {?MODULE, hook2, 0}]},
             {b, [{?MODULE, hook1, 2},
                  {?MODULE, hook2, 2}]},
             {c, [{?MODULE, hook_add, 1},
                  {?MODULE, hook_add1, 1}]}],
    ok = hooks:mreg(Hooks),
    ?assertEqual(ok, hooks:run(a, [])),
    ?assertEqual([1, 1], hooks:run_fold(a, [1], [])),
    ?assertEqual({ok, [{?MODULE, hook_add}, {?MODULE, hook_add1}]}, hooks_list:find(c)),
    ?assertEqual(4, hooks:run_fold(c, [], 1)),
    ok = hooks:munreg(Hooks),
    ok.

all_test() ->
    {ok, _} = application:ensure_all_started(hooks),

    Hooks = [{a, [{?MODULE, hook1, 0},
                  {?MODULE, hook2, 0}]},
             {b, [{?MODULE, hook1, 2},
                  {?MODULE, hook2, 2}]}],
    ok = hooks:mreg(Hooks),

    ?assertEqual([[], []], hooks:all(a, [])),
    ?assertEqual([[1 | 1], [1 | 1]], hooks:all(b, [1, 1])),
    ok = hooks:munreg(Hooks),
    ok.

all_till_ok_test() ->
    {ok, _} = application:ensure_all_started(hooks),

    Hooks = [{a, [{?MODULE, hooks_ok1, 0}]},
             {b, [{?MODULE, hooks_ok1, 0},
                  {?MODULE, hooks_ok2, 0}]},
             {c, [{?MODULE, hooks_ok1, 0},
                  {?MODULE, hooks_ok3, 0},
                  {?MODULE, hooks_ok4, 0}]}],

    ok = hooks:mreg(Hooks),
    ?assertEqual({error, [next]}, hooks:all_till_ok(a, [])),
    ?assertEqual(ok, hooks:all_till_ok(b, [])),
    ?assertEqual({ok, 0}, hooks:all_till_ok(c, [])),
    ok = hooks:munreg(Hooks),
    ok.

only_test() ->
    {ok, _} = application:ensure_all_started(hooks),

    ok = hooks:reg(a, ?MODULE, hook_add, 1, 10),
    ok = hooks:reg(a, ?MODULE, hook_add2, 1, 0),

    ?assertEqual({ok, [{?MODULE, hook_add2}, {?MODULE, hook_add}]},
                 hooks_list:find(a)),

    ?assertEqual(3, hooks:only(a, [1])),

    ok = hooks:unreg(a, ?MODULE, hook_add, 1, 10),
    ok = hooks:unreg(a, ?MODULE, hook_add2, 1, 0),
    ?assertEqual(hook_not_found, hooks_list:find(a)),
    ok.

plugin_test() ->
    {ok, _} = application:ensure_all_started(hooks),

    Hooks = [{a, [{?MODULE, hook1, 0},
                  {?MODULE, hook2, 0}]},
             {b, [{?MODULE, hook1, 2},
                  {?MODULE, hook2, 2}]}],

    application:set_env(hooks, hooks, Hooks),
    hooks:enable_plugin(hooks),

    FoundA = hooks_list:find(a),
    ?assertEqual({ok, [{?MODULE, hook1}, {?MODULE, hook2}]}, FoundA),
    FoundB = hooks_list:find(b),
    ?assertEqual({ok, [{?MODULE, hook1}, {?MODULE, hook2}]}, FoundB),
    ?assertEqual(ok, hooks:disable_plugin(hooks)),
    ?assertEqual(hook_not_found, hooks_list:find(a)),
    ?assertEqual(hook_not_found, hooks_list:find(b)),
    ?assertEqual(ok, hooks:disable_plugin(hooks)),
    ok.

init_test_wait_loop(Server) ->
    timer:sleep(300),
    register(test, self()),
    timer:sleep(200),
    Server ! self(),
    test_wait_loop().

test_wait_loop() ->
    timer:sleep(10),
    test_wait_loop().



wait_for_proc_test() ->
    error_logger:tty(false),
    application:stop(hooks),
    error_logger:tty(true),
    application:set_env(hooks, wait_for_proc, test),
    application:set_env(hooks, hooks, []),
    {ok, _} = application:ensure_all_started(hooks),

    Hooks = [{a, [{?MODULE, hook1, 0},
                  {?MODULE, hook2, 0}]},
             {b, [{?MODULE, hook1, 2},
                  {?MODULE, hook2, 2}]}],


    hooks:mreg(Hooks),

    ?assertEqual(hook_not_found, hooks_list:find(a)),
    Self = self(),
    Pid = spawn_link(fun() -> init_test_wait_loop(Self) end),
    receive
        Pid -> ok
    end,
    FoundA = hooks_list:find(a),
    ?assertEqual({ok, [{?MODULE, hook1}, {?MODULE, hook2}]}, FoundA),
    exit(Pid, normal),

    hooks:munreg(Hooks),
    ok.



-endif.


