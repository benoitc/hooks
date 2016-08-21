defmodule HooksMregTest do
    use ExUnit.Case, async: true

    setup_all do 
        Application.ensure_all_started(:hooks)
        hooks = [
                    {:a, a}, 
                    {:b, b}
                ]
        Hooks.mreg(hooks)
        {:ok, hooks: hooks}                
    end


    def hook1(args) do
        [ok: args]
    end

    def hook2(args) do
        [ok: args]
    end

    def hook2(args1, args2) do
        args1 + args2
    end

    def a do
        [{__MODULE__, :hook1, 1}, {__MODULE__, :hook1, 1}]
    end

    def b do
         [{__MODULE__, :hook2, 1}, {__MODULE__, :hook2, 1}]
    end

    test "uses mreg and finds b" do
        assert {:ok, [{__MODULE__, :hook2}, {__MODULE__, :hook2}]} == Hooks.find(:b)
    end

    test "uses mreg and runs all" do
        assert [[ok: 1], [ok: 1]] == Hooks.all(:b, [1])
    end

    test "uses mreg and run_fold" do
        assert 4 == Hooks.run_fold(:b, [1], 2) 
    end
end