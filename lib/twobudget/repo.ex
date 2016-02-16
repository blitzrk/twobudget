defmodule TwoBudget.Repo do
  use Ecto.Repo,
    otp_app: :twobudget,
    adapter: Sqlite.Ecto
end
