# credo:disable-for-this-file Credo.Check.Refactor.Nesting
with {:module, StreamData} <- Code.ensure_loaded(StreamData) do
  defmodule Purl.Generator.Version do
    @moduledoc false

    import StreamData

    @spec major :: StreamData.t(Version.major())
    defp major, do: non_negative_integer()

    @spec minor :: StreamData.t(Version.minor())
    defp minor, do: non_negative_integer()

    @spec patch :: StreamData.t(Version.patch())
    defp patch, do: non_negative_integer()

    @spec pre :: StreamData.t(Version.pre())
    defp pre, do: list_of(one_of([non_negative_integer(), string(:printable, max_length: 20)]), max_length: 5)

    @spec build :: StreamData.t(Version.build())
    defp build, do: one_of([constant(nil), string(:printable, max_length: 20)])

    @spec version :: StreamData.t(Version.t())
    def version do
      bind(major(), fn major ->
        bind(minor(), fn minor ->
          bind(patch(), fn patch ->
            bind(pre(), fn pre ->
              bind(build(), fn build ->
                constant(%Version{
                  build: build,
                  major: major,
                  minor: minor,
                  patch: patch,
                  pre: pre
                })
              end)
            end)
          end)
        end)
      end)
    end
  end
end
