defmodule Hooks.Mixfile do
  use Mix.Project

  @version "3.0.0"

  def project do
    [app: :hooks,
     version: @version,
     description: "Generic plugin & hook system",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),
     package: package(),
     docs: docs()]
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
    [
      {:ex_doc, "~> 0.31.0", only: :dev, runtime: false}
    ]
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
        "NEWS.md",
        "LICENSE"
      ],
      maintainers: ["Benoit Chesneau", "Sushruth Sivaramakrishnan"],
      licenses: ["BSD"],
      links: %{"Github" => "https://github.com/benoitc/hooks"}
    ]
  end

  defp docs do
    [
      extras: ["README.md", "NEWS.md", "LICENSE"],
      main: "readme",
      source_url: "https://github.com/benoitc/hooks",
      source_ref: "v#{@version}"
    ]
  end
end
