defmodule TwoBudget.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, "Hello world!")
  end

  match _ do
    send_resp(conn, 404, "not implemented")
  end
end
