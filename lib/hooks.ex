defmodule Hooks do
  @moduledoc """
  Elixir wrapper for the Erlang hooks system.

  ## Overview

  This module provides an Elixir-friendly interface to the hooks system,
  delegating all calls to the underlying Erlang implementation while
  maintaining Elixir naming conventions and documentation.

  ## Features

  - Priority-based hook execution
  - Multiple execution strategies
  - Plugin system support
  - Zero overhead - direct delegation to Erlang

  ## Examples

  ### Basic Usage

      # Register a hook
      :ok = Hooks.reg(:on_user_login, MyModule, :log_login, 2, 10)

      # Run hooks
      :ok = Hooks.run(:on_user_login, [user_id, timestamp])

      # Unregister a hook
      :ok = Hooks.unreg(:on_user_login, MyModule, :log_login, 2)

  ### Using Plugins

      # Enable a plugin
      :ok = Hooks.enable_plugin(:my_plugin)

      # Disable a plugin
      :ok = Hooks.disable_plugin(:my_plugin)

  ### Different Execution Strategies

      # Run all hooks until one returns :stop
      :ok = Hooks.run(:before_save, [data])

      # Fold over hooks with accumulator
      result = Hooks.run_fold(:transform_data, [input], initial_acc)

      # Get all results
      results = Hooks.all(:validate_data, [data])

      # Run until one succeeds
      case Hooks.all_till_ok(:authenticate, [user, pass]) do
        :ok -> :authenticated
        {:ok, user_info} -> {:authenticated, user_info}
        {:error, reasons} -> {:failed, reasons}
      end

  ## Since

  1.0.0
  """

  @doc """
  Register a function as a hook using the function name as the hook name.

  ## Parameters

    * `module` - The module containing the hook function
    * `fun` - The function name (also used as the hook name)
    * `arity` - The function arity

  ## Returns

    * `:ok` - Registration successful
    * `{:error, term}` - Registration failed

  ## Examples

      :ok = Hooks.reg(MyModule, :on_startup, 0)

  """
  defdelegate reg(module, fun, arity), to: :hooks

  @doc """
  Register a function for a specific hook name.

  ## Parameters

    * `name` - The hook identifier
    * `module` - The module containing the hook function
    * `fun` - The function name
    * `arity` - The function arity

  ## Returns

    * `:ok` - Registration successful
    * `{:error, term}` - Registration failed

  ## Examples

      :ok = Hooks.reg(:before_save, Validator, :check_data, 1)

  """
  defdelegate reg(name, module, fun, arity), to: :hooks

  @doc """
  Register a function for a hook with a specific priority.

  ## Parameters

    * `name` - The hook identifier
    * `module` - The module containing the hook function
    * `fun` - The function name
    * `arity` - The function arity
    * `priority` - Execution priority (higher values execute first)

  ## Returns

    * `:ok` - Registration successful
    * `{:error, term}` - Registration failed

  ## Examples

      # High priority (executes first)
      :ok = Hooks.reg(:before_save, Security, :check_permissions, 1, 100)

      # Normal priority
      :ok = Hooks.reg(:before_save, Validator, :validate_data, 1, 0)

      # Low priority (executes last)
      :ok = Hooks.reg(:before_save, Logger, :log_save, 1, -10)

  """
  defdelegate reg(name, module, fun, arity, priority), to: :hooks

  @doc """
  Unregister a function where the function name is the hook name.

  ## Parameters

    * `module` - The module containing the hook function
    * `fun` - The function name (also the hook name)
    * `arity` - The function arity

  ## Returns

  Always returns `:ok`.

  ## Examples

      :ok = Hooks.unreg(MyModule, :on_startup, 0)

  """
  defdelegate unreg(module, fun, arity), to: :hooks

  @doc """
  Unregister a function from a specific hook.

  ## Parameters

    * `name` - The hook identifier
    * `module` - The module containing the hook function
    * `fun` - The function name
    * `arity` - The function arity

  ## Returns

  Always returns `:ok`.

  ## Examples

      :ok = Hooks.unreg(:before_save, Validator, :check_data, 1)

  """
  defdelegate unreg(name, module, fun, arity), to: :hooks

  @doc """
  Unregister a function from a hook with a specific priority.

  ## Parameters

    * `name` - The hook identifier
    * `module` - The module containing the hook function
    * `fun` - The function name
    * `arity` - The function arity
    * `priority` - The priority used during registration

  ## Returns

  Always returns `:ok`.

  ## Examples

      :ok = Hooks.unreg(:before_save, Security, :check_permissions, 1, 100)

  """
  defdelegate unreg(name, module, fun, arity, priority), to: :hooks

  @doc """
  Register multiple hooks at once.

  ## Parameters

    * `hooks` - A list of `{hook_name, hook_functions}` tuples

  ## Returns

    * `:ok` - All registrations successful
    * `{:error, term}` - At least one registration failed

  ## Examples

      hooks = [
        {:on_connect, [{Logger, :log_connection, 1}, {Stats, :count_connection, 1}]},
        {:on_disconnect, [{Logger, :log_disconnection, 1}]}
      ]
      :ok = Hooks.mreg(hooks)

  """
  defdelegate mreg(hooks), to: :hooks

  @doc """
  Unregister multiple hooks at once.

  ## Parameters

    * `hooks` - A list of `{hook_name, hook_functions}` tuples

  ## Returns

  Always returns `:ok`.

  ## Examples

      hooks = [
        {:on_connect, [{Logger, :log_connection, 1}]},
        {:on_disconnect, [{Logger, :log_disconnection, 1}]}
      ]
      :ok = Hooks.munreg(hooks)

  """
  defdelegate munreg(hooks), to: :hooks

  @doc """
  Retrieve all functions registered for a hook.

  ## Parameters

    * `atom` - The hook identifier

  ## Returns

    * `{:ok, [{module, function}]}` - List of registered functions
    * `:error` - If no hooks are registered

  ## Examples

      case Hooks.find(:on_user_login) do
        {:ok, hooks} ->
          IO.inspect(hooks, label: "Registered hooks")
        :error ->
          IO.puts("No hooks registered")
      end

  """
  defdelegate find(atom), to: :hooks

  @doc """
  Execute all hooks and collect all results.

  ## Parameters

    * `atom` - The hook identifier
    * `list` - Arguments to pass to each hook

  ## Returns

  A list containing the return value of each hook function.

  ## Examples

      results = Hooks.all(:validate_user, [user_data])
      case Enum.all?(results, & &1 == :ok) do
        true -> :proceed
        false -> {:error, :validation_failed}
      end

  """
  defdelegate all(atom, list), to: :hooks

  @doc """
  Execute hooks until one returns success.

  ## Parameters

    * `atom` - The hook identifier
    * `list` - Arguments to pass to each hook

  ## Returns

    * `:ok` - If any hook returns `:ok`
    * `{:ok, value}` - If any hook returns `{:ok, value}`
    * `{:error, [term]}` - List of all non-ok results
    * `[]` - If no hooks are registered

  ## Examples

      case Hooks.all_till_ok(:authenticate, [username, password]) do
        :ok ->
          login_user(username)
        {:ok, user_info} ->
          login_user_with_info(username, user_info)
        {:error, reasons} ->
          {:error, {:auth_failed, reasons}}
      end

  """
  defdelegate all_till_ok(atom, list), to: :hooks

  @doc """
  Disable a plugin and unregister all its hooks.

  ## Parameters

    * `atom` - The plugin application name

  ## Returns

  Always returns `:ok`.

  ## Examples

      :ok = Hooks.disable_plugin(:my_plugin)

  """
  defdelegate disable_plugin(atom), to: :hooks

  @doc """
  Enable a plugin application and register its hooks.

  ## Parameters

    * `atom` - The plugin application name

  ## Returns

    * `:ok` - Plugin enabled successfully
    * `{:error, term}` - Failed to enable plugin

  ## Examples

      :ok = Hooks.enable_plugin(:my_plugin)

  """
  defdelegate enable_plugin(atom), to: :hooks

  @doc """
  Enable a plugin with additional code paths.

  ## Parameters

    * `atom` - The plugin application name
    * `list` - Additional code paths

  ## Returns

    * `:ok` - Plugin enabled successfully
    * `{:error, term}` - Failed to enable plugin

  ## Examples

      :ok = Hooks.enable_plugin(:my_plugin, ["./plugins/my_plugin/ebin"])

  """
  defdelegate enable_plugin(atom, list), to: :hooks

  @doc """
  Execute only the highest priority hook.

  ## Parameters

    * `atom` - The hook identifier
    * `list` - Arguments to pass to the hook

  ## Returns

    * The return value of the highest priority hook
    * `:no_hook` - If no hooks are registered

  ## Examples

      case Hooks.only(:custom_handler, [request]) do
        :no_hook ->
          handle_default(request)
        result ->
          result
      end

  """
  defdelegate only(atom, list), to: :hooks

  @doc """
  Run all hooks registered for a hook name.

  ## Parameters

    * `atom` - The hook identifier
    * `list` - Arguments to pass to each hook

  ## Returns

  Always returns `:ok`.

  ## Notes

  Execution stops if any hook returns `:stop`.

  ## Examples

      :ok = Hooks.run(:on_user_login, [user_id, timestamp])

  """
  defdelegate run(atom, list), to: :hooks

  @doc """
  Fold over all hooks with an accumulator.

  ## Parameters

    * `atom` - The hook identifier
    * `list` - Arguments to pass to each hook
    * `any` - Initial accumulator value

  ## Returns

  The final accumulator value.

  ## Notes

  - Each hook receives args ++ [accumulator]
  - Return value becomes next accumulator
  - Stops on `:stop` or `{:stop, new_acc}`

  ## Examples

      processed_data = Hooks.run_fold(:transform_data, [options], initial_data)

  """
  defdelegate run_fold(atom, list, any), to: :hooks
end