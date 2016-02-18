defmodule TwoBudget.Router do
  use Plug.Router

  @config Application.get_env(:twobudget, :http)

  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, "Hello world!")
  end

  get "/login" do
    page_contents = EEx.eval_file("templates/login.eex", [])
    conn
    |> put_resp_content_type("text/html")
    |> send_resp(200, page_contents)
  end

  match _ do
    send_resp(conn, 404, "Not found")
  end

  def run() do
    IO.puts "Starting app at localhost:#{@config[:port]}"
    {:ok, _} = Plug.Adapters.Cowboy.http __MODULE__, [], @config
  end
end
