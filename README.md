

# hooks - Generic Hooks system for Erlang applications #

Copyright (c) 2015 Benoît Chesneau.

__Version:__ 1.0.2

# hooks

`hooks` is a generic Hooks system for Erlang applications. It allows you to
augment your application by adding hooks to your application aka
[Hooking](https://en.wikipedia.org/wiki/Hookin://en.wikipedia.org/wiki/Hooking).

Main Features are:

- Handle module hooks
- Basic plugin system
- Module list is compiled to a beam once an hook or a list of hooks is registered. It allows us to share the list of registered hooks between every process of your application without message passing. It is also memory efficient and minimize locking.

This application is inspired by the [vmq_plugins](https://github.com/erlio/vmq_plugin) but is rewritten from scratch to use merl, and simplify its usage.

## Usage

Full application API is available in [`hooks`](http://github.com/barrel-db/hooks/blob/master/doc/hooks.md) .

### adding hooks manually

Your application can add hooks using the following methods

- `hooks:reg/{3, 4, 5}` to register a hook from a module
- `hook:unreg/{3, 5, 5}` to unregister a hook.

```
ok = hooks:reg(a, ?MODULE, hook_add, 1, 10),
ok = hooks:reg(a, ?MODULE, hook_add2, 1, 0),
```

### add multiple hooks

- `hook:mreg/1`: to register multiple hooks
- `hooks:munreg/1`: to register multiple hooks

Ex:

```
Hooks = [{a, [{?MODULE, hook1, 0},
              {?MODULE, hook2, 0}]},
         {b, [{?MODULE, hook1, 2},
              {?MODULE, hook2, 2}]},
         {c, [{?MODULE, hook_add, 1},
              {?MODULE, hook_add1, 1}]}],
%% register multiple hooks
ok = hooks:mreg(Hooks),
%% unregister multiple hooks
ok = hooks:munreg(Hooks)
```

### Enable/Disable Plugins

Plugins are simple Erlang applications that exposes hooks. A plugin can be enabled to your application using `hooks:enable_plugin/{1,2}` and disabled using `hooks:disable_plugin/1` . When enabled the application and its dependencies are started (if not already stared) and exposed hooks are registered.

To expose the hooks,  just add them to application environment settings. Example:

```
{application, 'myapp',
 [{description, ""},
  {vsn, "1.0.0"},

  ...

  {env,[
    {hooks, [{a, [{?MODULE, hook1, 0},
                  {?MODULE, hook2, 0}]},
             {b, [{?MODULE, hook1, 2},
                  {?MODULE, hook2, 2}]}]},
    ...
  ]},

  ...

 ]}.
```

> You can specify a patch where to load the application and its dependencies.

### run hooks

You can use the following command to execute hooks

- `hooks:run/2` :run all hooks registered for the HookName.
- `hooks:run_fold/3`: fold over all hooks registered for HookName, and return Acc.
- `hooks:all/2`: execute all hooks for this HookName and return all results
- `hooks:all_till_ok/2`: execute all hooks for the HookName until one return ok or {ok, Val}.
- `hooks:only/2`: call the top priority hook for the HookName and return the result.

### advanced features

2 internal hooks are exposed:

- `init_hooks`: the hook is executed when hooks is started, a function of arity 0 is expected.
- `build_hooks`: the hooks is executed when the list of hooks has changed. A function of arity 1, receiving the list of hooks is expected.

When added to the hooks application environnement, the hooks are immediately available and won't wait for any registered process.

the `wait_for_proc` apllication environnement settings in the `hooks` application allows you to wait for a specific registered process (example your main application process) to be started before making the hooks available. It means that until the process isn`t registered the beam containing the list of hooks won't be compiled with the list of added hooks.

