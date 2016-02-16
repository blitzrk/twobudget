defmodule TwoBudget.Web.Supervisor do
  def start_link() do
    opts = Application.get_env(:twobudget, :http)
    {:ok, sup} = Plug.Adapters.Cowboy.http TwoBudget.Router, [], opts

    IO.puts "Starting server on localhost:#{opts[:port]}"
    {:ok, sup}
  end
end
