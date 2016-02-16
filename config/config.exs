use Mix.Config

defmodule Config do
  def port() do
    port = System.get_env("PORT")
    if port do
      {port, _} = Integer.parse(port)
      port
    else
      4000
    end
  end
end

config :twobudget, TwoBudget.Repo,
  adapter: Ecto.Sqlite,
  database: "budget.sqlite3"

config :twobudget, :http,
  port: Config.port

#import_config "#{Mix.env}.exs"
