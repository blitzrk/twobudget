defmodule TwoBudget.PageController do
  use TwoBudget.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
