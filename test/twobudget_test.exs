defmodule TwoBudgetTest do
  use ExUnit.Case
  use Plug.Test
  alias TwoBudget.Repo
  alias Ecto.Adapters.SQL

  setup do
    SQL.begin_test_transaction(Repo)

    on_exit fn ->
      SQL.rollback_test_transaction(Repo)
    end
  end
end
