%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Oct 2017 12:29
%%%-------------------------------------------------------------------
-module(hooks_test_SUITE).
-author("benoitc").

-compile(export_all).

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/1
]).



all() ->
  [
    basic_test,
    mreg_test,
    run_test,
    all_test,
    all_till_ok_test,
    only_test,
    plugin_test,
    wait_for_proc_test
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(hooks),
  Config.

end_per_suite(Config) ->
  _ = application:stop(hooks),
  Config.


init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_Config) ->
  ok.




%% hooks for tests

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


%% tests


basic_test(_) ->
  {ok, _} = application:ensure_all_started(hooks),
  
  Hooks = [{a, [{?MODULE, hook1, 0},
                {?MODULE, hook2, 0}]},
           {b, [{?MODULE, hook1, 2},
                {?MODULE, hook2, 2}]}],
  
  lists:foreach(
    fun({Name, Specs}) ->
      [ok, ok] = [hooks:reg(Name, M, F, A) || {M, F, A} <- Specs]
    end,
    Hooks),
  
  {ok, [{?MODULE, hook1}, {?MODULE, hook2}]} = hooks:find(a),
  {ok, [{?MODULE, hook1}, {?MODULE, hook2}]} =  hooks:find(b),
  
  
  error = hooks:find(c),
  ok = hooks:reg(c, ?MODULE, hook3, 3),
  {ok, [{?MODULE, hook3}]} = hooks:find(c),
  ok = hooks:unreg(c, ?MODULE, hook3, 3),
  error =  hooks:find(c),
  {error, hooks_not_exported} = hooks:reg(c, ?MODULE, hook3, 2),
  
  lists:foreach(
    fun({Name, Specs}) ->
      [ok, ok] = [hooks:unreg(Name, M, F, A) || {M, F, A} <- Specs]
    end,
    Hooks),
  error = hooks:find(a),
  error = hooks:find(b),
  ok.

mreg_test(_) ->
  {ok, _} = application:ensure_all_started(hooks),
  
  Hooks = [{a, [{?MODULE, hook1, 0},
                {?MODULE, hook2, 0}]},
           {b, [{?MODULE, hook1, 2},
                {?MODULE, hook2, 2}]}],
  ok = hooks:mreg(Hooks),
  {ok, [{?MODULE, hook1}, {?MODULE, hook2}]} = hooks:find(a),
  {ok, [{?MODULE, hook1}, {?MODULE, hook2}]} = hooks:find(b),
  ok = hooks:munreg(Hooks),
  error = hooks:find(a),
  error = hooks:find(b),
  ok.

run_test(_) ->
  {ok, _} = application:ensure_all_started(hooks),
  
  Hooks = [{a, [{?MODULE, hook1, 0},
                {?MODULE, hook2, 0}]},
           {b, [{?MODULE, hook1, 2},
                {?MODULE, hook2, 2}]},
           {c, [{?MODULE, hook_add, 1},
                {?MODULE, hook_add1, 1}]}],
  ok = hooks:mreg(Hooks),
  ok = hooks:run(a, []),
  [1, 1] = hooks:run_fold(a, [1], []),
  {ok, [{?MODULE, hook_add}, {?MODULE, hook_add1}]} = hooks:find(c),
  4 = hooks:run_fold(c, [], 1),
  ok = hooks:munreg(Hooks),
  ok.

all_test(_) ->
  {ok, _} = application:ensure_all_started(hooks),
  
  Hooks = [{a, [{?MODULE, hook1, 0},
                {?MODULE, hook2, 0}]},
           {b, [{?MODULE, hook1, 2},
                {?MODULE, hook2, 2}]}],
  ok = hooks:mreg(Hooks),
  
  [[], []] = hooks:all(a, []),
  [[1 | 1], [1 | 1]] = hooks:all(b, [1, 1]),
  ok = hooks:munreg(Hooks),
  ok.

all_till_ok_test(_) ->
  {ok, _} = application:ensure_all_started(hooks),
  
  Hooks = [{a, [{?MODULE, hooks_ok1, 0}]},
           {b, [{?MODULE, hooks_ok1, 0},
                {?MODULE, hooks_ok2, 0}]},
           {c, [{?MODULE, hooks_ok1, 0},
                {?MODULE, hooks_ok3, 0},
                {?MODULE, hooks_ok4, 0}]}],
  
  ok = hooks:mreg(Hooks),
  {error, [next]} = hooks:all_till_ok(a, []),
  ok = hooks:all_till_ok(b, []),
  {ok, 0} = hooks:all_till_ok(c, []),
  ok = hooks:munreg(Hooks),
  ok.

only_test(_) ->
  {ok, _} = application:ensure_all_started(hooks),
  
  ok = hooks:reg(a, ?MODULE, hook_add, 1, 10),
  ok = hooks:reg(a, ?MODULE, hook_add2, 1, 0),
  
  {ok, [{?MODULE, hook_add2}, {?MODULE, hook_add}]} = hooks:find(a),
  
  3 = hooks:only(a, [1]),
  
  ok = hooks:unreg(a, ?MODULE, hook_add, 1, 10),
  ok = hooks:unreg(a, ?MODULE, hook_add2, 1, 0),
  error = hooks:find(a),
  ok.

plugin_test(_) ->
  {ok, _} = application:ensure_all_started(hooks),
  
  Hooks = [{a, [{?MODULE, hook1, 0},
                {?MODULE, hook2, 0}]},
           {b, [{?MODULE, hook1, 2},
                {?MODULE, hook2, 2}]}],
  
  application:set_env(hooks, hooks, Hooks),
  hooks:enable_plugin(hooks),
  
  {ok, [{?MODULE, hook1}, {?MODULE, hook2}]} = hooks:find(a),
  {ok, [{?MODULE, hook1}, {?MODULE, hook2}]} = hooks:find(b),
  ok = hooks:disable_plugin(hooks),
  error = hooks:find(a),
  error = hooks:find(b),
  ok = hooks:disable_plugin(hooks),
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

wait_for_proc_test(_) ->
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
  
  error = hooks:find(a) ,
  Self = self(),
  Pid = spawn_link(fun() -> init_test_wait_loop(Self) end),
  receive
    Pid -> ok
  end,
  FoundA = hooks:find(a),
  {ok, [{?MODULE, hook1}, {?MODULE, hook2}]} = FoundA,
  exit(Pid, normal),
  
  hooks:munreg(Hooks),
  ok.

