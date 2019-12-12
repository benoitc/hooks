defmodule Hooks.Mixfile do
  use Mix.Project

  def project do
    [app: :hooks,
     version: "2.1.0",
     description: "Generic plugin & hook system",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),
     package: package()]
  end

  def application do
    [
      applications:
        [
          :kernel,
          :stdlib,
        ],
      mod: {:hooks_app, []},
      registered: [:hooks_sup]
    ]
  end

  defp deps do
    []
  end

  def package do
    [
      files: [
        "src",
        "lib",
        "mix.exs",
        "mix.lock",
        "rebar.config",
        "rebar.lock",
        "README.md",
        "LICENSE"
      ],
      maintainers: ["Benoit Chesneau", "Sushruth Sivaramakrishnan"],
      licenses: ["BSD"],
      links: %{"Github" => "https://github.com/barrel-db/hooks"}
    ]
  end
end
