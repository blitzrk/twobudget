defmodule TwoBudget do
  use Application

  def start(_type, _args) do
    TwoBudget.Supervisor.start_link
    TwoBudget.Router.run
  end
end
