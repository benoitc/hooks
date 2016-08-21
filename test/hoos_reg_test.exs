defmodule HooksRegTest do
    use ExUnit.Case, async: true

    setup_all do 
        Application.ensure_all_started(:hooks)
        name = :c
        {module, fun, arity} = c
        hook = {name,  c}
        Hooks.reg(name, module, fun, arity)
        {:ok, hook: hook}                
    end

    def hook1(args) do
        [ok: args]
    end

    def hook1(args1, args2) do
        args1 + args2
    end

    def c do
        {__MODULE__, :hook1, 1}
    end

    test "uses reg and finds a" do
        assert {:ok, [{__MODULE__, :hook1}]} == Hooks.find(:c)
    end

    test "uses reg and runs all" do
        assert [[ok: 1]] == Hooks.all(:c, [1])
    end

    test "uses reg and run_fold" do
        assert 3 == Hooks.run_fold(:c, [1], 2) 
    end
end