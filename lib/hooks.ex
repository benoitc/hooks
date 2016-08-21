defmodule Hooks do
    defdelegate reg(module, fun, arity), to: :hooks
    defdelegate reg(name, module, fun, arity), to: :hooks
    defdelegate reg(name, module, fun, arity, priority), to: :hooks
    defdelegate unreg(module, fun, arity), to: :hooks
    defdelegate unreg(name, module, fun, arity), to: :hooks
    defdelegate unreg(name, module, fun, arity, priority), to: :hooks
    defdelegate mreg(hooks), to: :hooks
    defdelegate munreg(hooks), to: :hooks
    defdelegate find(atom), to: :hooks
    defdelegate all(atom, list), to: :hooks
    defdelegate all_till_ok(atom, list), to: :hooks
    defdelegate disable_plugin(atom), to: :hooks
    defdelegate enable_plugin(atom), to: :hooks
    defdelegate enable_plugin(atom, list), to: :hooks
    defdelegate only(atom, list), to: :hooks
    defdelegate run(atom ,list), to: :hooks
    defdelegate run_fold(atom, list, any), to: :hooks
end