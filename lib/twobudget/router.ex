defmodule TwoBudget.Router do
  use Plug.Router

  @config Application.get_env(:twobudget, :http)

  plug Plug.Static, at: "/static", from: :twobudget
  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, "Hello world!")
  end

  forward "/login", to: TwoBudget.LoginRouter

  match _ do
    send_resp(conn, 404, "Not found")
  end

  def run() do
    IO.puts "Starting app at localhost:#{@config[:port]}"
    {:ok, _} = Plug.Adapters.Cowboy.http __MODULE__, [], @config
  end
end
