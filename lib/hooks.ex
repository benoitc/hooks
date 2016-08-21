defmodule Hooks do
    defdelegate reg(atom, atom, integer), to: :hooks
    defdelegate reg(atom, atom, atom, integer), to: :hooks
    defdelegate reg(atom, atom, atom, integer, integer), to: :hooks
    defdelegate unreg(atom, atom, integer), to: :hooks
    defdelegate unreg(atom, atom, atom, integer), to: :hooks
    defdelegate unreg(atom, atom, atom, integer, integer), to: :hooks
    defdelegate mreg(list), to: :hooks
    defdelegate munreg(list), to: :hooks
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