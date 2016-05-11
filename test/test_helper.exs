ExUnit.start

Mix.Task.run "ecto.create", ~w(-r TwoBudget.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r TwoBudget.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(TwoBudget.Repo)

