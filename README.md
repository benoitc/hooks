# hooks

[![Build Status](https://github.com/benoitc/hooks/workflows/CI/badge.svg)](https://github.com/benoitc/hooks/actions)
[![Hex.pm](https://img.shields.io/hexpm/v/hooks.svg)](https://hex.pm/packages/hooks)
[![Documentation](https://img.shields.io/badge/docs-hexdocs-blue.svg)](https://hexdocs.pm/hooks/)

A generic and efficient Hooks system for Erlang and Elixir applications.

## Overview

`hooks` is a generic hooks system that allows you to augment your application by adding hooks (also known as extension points or plugins). It's designed to be efficient and minimalistic while providing powerful plugin capabilities.

### Main Features

- **Simple Hook Registration** - Register hooks with priority-based execution
- **Plugin System** - Enable/disable plugins dynamically
- **Efficient Storage** - Uses persistent_term for constant-time hook lookups
- **Multiple Execution Modes** - Run all, run until ok, run fold, or run only the first hook
- **Erlang & Elixir Support** - Native support for both languages
- **No Dependencies** - Zero runtime dependencies

## Installation

### Erlang (rebar3)

Add `hooks` to your `rebar.config` dependencies:

```erlang
{deps, [
    {hooks, "3.0.0"}
]}.
```

### Elixir (mix)

Add `hooks` to your `mix.exs` dependencies:

```elixir
def deps do
  [
    {:hooks, "~> 3.0.0"}
  ]
end
```

## Quick Start

### Erlang Example

```erlang
%% Start the hooks application
application:ensure_all_started(hooks).

%% Register a hook
ok = hooks:reg(my_hook, my_module, my_function, 1, 0).

%% Run the hook
ok = hooks:run(my_hook, [arg1]).

%% Unregister the hook
ok = hooks:unreg(my_hook, my_module, my_function, 1).
```

### Elixir Example

```elixir
# Start the hooks application
Application.ensure_all_started(:hooks)

# Register a hook
:ok = Hooks.reg(:my_hook, MyModule, :my_function, 1)

# Run the hook
:ok = Hooks.run(:my_hook, [:arg1])

# Unregister the hook
:ok = Hooks.unreg(:my_hook, MyModule, :my_function, 1)
```

## API Documentation

### Hook Registration

#### `reg/3`, `reg/4`, `reg/5`

Register a hook function.

```erlang
%% Register using function name as hook name
hooks:reg(Module, Function, Arity).

%% Register with specific hook name
hooks:reg(HookName, Module, Function, Arity).

%% Register with priority (default is 0, higher priority executes first)
hooks:reg(HookName, Module, Function, Arity, Priority).
```

#### `unreg/3`, `unreg/4`, `unreg/5`

Unregister a hook function.

```erlang
hooks:unreg(Module, Function, Arity).
hooks:unreg(HookName, Module, Function, Arity).
hooks:unreg(HookName, Module, Function, Arity, Priority).
```

#### `mreg/1`, `munreg/1`

Register or unregister multiple hooks at once.

```erlang
%% Register multiple hooks
Hooks = [
    {hook_name1, [{Module1, Function1, Arity1}, {Module2, Function2, Arity2}]},
    {hook_name2, [{Module3, Function3, Arity3}]}
],
ok = hooks:mreg(Hooks).

%% Unregister multiple hooks
ok = hooks:munreg(Hooks).
```

### Hook Execution

#### `run/2`

Execute all hooks for a given hook name. Execution stops if a hook returns `stop`.

```erlang
ok = hooks:run(HookName, Args).
```

#### `run_fold/3`

Fold over all hooks, passing an accumulator.

```erlang
Result = hooks:run_fold(HookName, Args, InitialAcc).
```

#### `all/2`

Execute all hooks and return all results.

```erlang
Results = hooks:all(HookName, Args).
```

#### `all_till_ok/2`

Execute hooks until one returns `ok` or `{ok, Value}`.

```erlang
Result = hooks:all_till_ok(HookName, Args).
```

#### `only/2`

Execute only the highest priority hook.

```erlang
Result = hooks:only(HookName, Args).
```

### Plugin Management

#### `enable_plugin/1`, `enable_plugin/2`

Enable a plugin (Erlang application) and register its hooks.

```erlang
%% Enable plugin
ok = hooks:enable_plugin(my_plugin).

%% Enable plugin with custom code paths
ok = hooks:enable_plugin(my_plugin, ["path/to/plugin"]).
```

Plugins expose hooks through their application environment:

```erlang
%% In plugin's .app.src file
{env, [
    {hooks, [
        {on_connect, [{my_plugin, handle_connect, 2}]},
        {on_disconnect, [{my_plugin, handle_disconnect, 1}]}
    ]}
]}
```

#### `disable_plugin/1`

Disable a plugin and unregister its hooks.

```erlang
ok = hooks:disable_plugin(my_plugin).
```

### Utility Functions

#### `find/1`

Find all registered hooks for a hook name.

```erlang
{ok, [{Module, Function}, ...]} = hooks:find(HookName).
error = hooks:find(NonExistentHook).
```

## Advanced Features

### Internal Hooks

Two internal hooks are available for the hooks application itself:

- `init_hooks` - Called when hooks application starts (arity 0)
- `build_hooks` - Called when the hook list changes (arity 1, receives hook list)

### Wait for Process

The `wait_for_proc` application environment setting allows hooks to wait for a specific registered process before becoming available:

```erlang
%% In your app.config or sys.config
[
    {hooks, [
        {wait_for_proc, my_main_process}
    ]}
].
```

This ensures hooks aren't executed until your main application process is ready.

## Examples

### Authentication Hook System

```erlang
%% Define authentication modules
-module(basic_auth).
-export([authenticate/2]).

authenticate(Username, Password) ->
    %% Basic authentication logic
    case check_credentials(Username, Password) of
        true -> ok;
        false -> {error, invalid_credentials}
    end.

-module(ldap_auth).
-export([authenticate/2]).

authenticate(Username, Password) ->
    %% LDAP authentication logic
    case ldap:check(Username, Password) of
        {ok, _} -> ok;
        _ -> {error, ldap_auth_failed}
    end.

%% Register authentication hooks
hooks:reg(authenticate, basic_auth, authenticate, 2, 10).
hooks:reg(authenticate, ldap_auth, authenticate, 2, 5).

%% Use authentication
case hooks:all_till_ok(authenticate, [Username, Password]) of
    ok -> login_success();
    {ok, UserData} -> login_success(UserData);
    {error, Reasons} -> login_failed(Reasons)
end.
```

### Event Processing Pipeline

```erlang
%% Define event processors
-module(logger).
-export([process_event/1]).

process_event(Event) ->
    error_logger:info_msg("Event: ~p~n", [Event]),
    Event.

-module(metrics).
-export([process_event/1]).

process_event(Event) ->
    prometheus:increment(event_counter),
    Event.

-module(validator).
-export([process_event/1]).

process_event(Event) ->
    case is_valid(Event) of
        true -> Event;
        false -> stop  %% Stop processing invalid events
    end.

%% Register event processors
hooks:reg(on_event, validator, process_event, 1, 100).  %% Highest priority
hooks:reg(on_event, logger, process_event, 1, 50).
hooks:reg(on_event, metrics, process_event, 1, 10).

%% Process an event
hooks:run(on_event, [Event]).
```

### Plugin-based Extension System

```erlang
%% my_plugin.app.src
{application, my_plugin,
 [{description, "My plugin"},
  {vsn, "1.0.0"},
  {modules, []},
  {env, [
    {hooks, [
        {startup, [{my_plugin, on_startup, 0}]},
        {shutdown, [{my_plugin, on_shutdown, 0}]},
        {request, [{my_plugin, on_request, 2}]}
    ]}
  ]}
]}.

%% Enable the plugin
hooks:enable_plugin(my_plugin).

%% The hooks are now automatically registered and will be called
hooks:run(startup, []).
```

## Performance Considerations

1. **Hook Storage**: Hooks are stored in ETS during registration and in persistent_term for execution, providing constant-time lookups.

2. **Compilation**: No dynamic module compilation - all hooks are immediately available.

3. **Memory**: Very memory efficient as persistent_term is optimized for read-heavy workloads.

4. **Concurrency**: Multiple processes can execute hooks simultaneously without locks.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

Copyright (c) 2015-2025 Benoît Chesneau.

This project is licensed under the BSD License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Originally created for the [barrel-db](https://github.com/barrel-db) project
- Inspired by various plugin systems in the Erlang ecosystem
- Thanks to all contributors who have helped improve this library

## Resources

- [Hex Package](https://hex.pm/packages/hooks)
- [Documentation](https://hexdocs.pm/hooks/)
- [GitHub Repository](https://github.com/benoitc/hooks)
- [Issue Tracker](https://github.com/benoitc/hooks/issues)