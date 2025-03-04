# credo:disable-for-this-file Credo.Check.Readability.Specs
defmodule Purl.MixProject do
  use Mix.Project

  @version "0.1.1"
  @source_url "https://github.com/maennchen/purl"
  @description "Implementation of the purl (package url) specification"

  def project do
    [
      app: :purl,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      test_coverage: [tool: ExCoveralls],
      description: @description,
      dialyzer:
        [list_unused_filters: true] ++
          if (System.get_env("DIALYZER_PLT_PRIV") || "false") in ["1", "true"] do
            [plt_file: {:no_warn, "priv/plts/dialyzer.plt"}]
          else
            []
          end,
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.html": :test,
        "coveralls.json": :test,
        "coveralls.post": :test,
        "coveralls.xml": :test
      ],
      package: package()
    ]
  end

  defp package do
    %{
      licenses: ["Apache-2.0"],
      maintainers: ["Jonatan Männchen"],
      links: %{
        "GitHub" => @source_url,
        "Changelog" => @source_url <> "/blob/main/CHANGELOG.md",
        "Issues" => @source_url <> "/issues",
        "purl Specification" => "https://github.com/package-url/purl-spec"
      }
    }
  end

  def application do
    []
  end

  defp docs do
    [
      source_url: @source_url,
      source_ref: "v" <> @version,
      main: "readme",
      extras: ["README.md"]
    ]
  end

  defp deps do
    [
      {:credo, "~> 1.0", only: [:dev], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:excoveralls, "~> 0.5", only: [:test], runtime: false},
      {:ex_doc, ">= 0.0.0", only: [:dev], runtime: false},
      {:jason, "~> 1.4", only: [:dev, :test]},
      {:styler, "~> 1.4", only: [:dev, :test], runtime: false}
    ]
  end
end
