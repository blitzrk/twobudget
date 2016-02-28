defmodule TwoBudget.LoginRouter do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    page_contents = EEx.eval_file("templates/login.eex", [])
    conn
    |> put_resp_content_type("text/html")
    |> send_resp(200, page_contents)
  end
end
