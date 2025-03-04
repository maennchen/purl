import Config

case System.get_env("CI", "0") do
  truthy when truthy in ~w[1 yes true] ->
    config :stream_data, max_runs: 20_000

  _falsy ->
    :ok
end
