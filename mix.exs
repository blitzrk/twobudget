defmodule TwoBudget.Mixfile do
  use Mix.Project

  def project do
    [app: :twobudget,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [ applications:
      [:logger, :cowboy, :plug, :sqlite_ecto, :ecto, :comeonin],
      mod: {TwoBudget, []}
    ]
  end

  defp deps do
    [ {:cowboy, "~> 1.0.0"},
      {:plug, "~> 1.0"},
      {:sqlite_ecto, "~> 1.0.0"},
      {:comeonin, "~> 2.1"}
    ]
  end
end
