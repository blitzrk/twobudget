defmodule TwoBudget do
  @moduledoc ~s"""
  """

  use Application

  def start(_type, _args) do
    TwoBudget.Supervisor.start_link
  end
end
