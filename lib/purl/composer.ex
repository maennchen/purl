defmodule Purl.Composer do
  @moduledoc false

  @spec compose_uri(purl :: Purl.t()) :: URI.t()
  def compose_uri(purl)

  def compose_uri(%Purl{
        type: type,
        namespace: namespace,
        name: name,
        version: version,
        qualifiers: qualifiers,
        subpath: subpath
      }) do
    %URI{
      scheme: "pkg",
      path: type |> encode_uri_path(namespace, name, version) |> IO.iodata_to_binary(),
      query: encode_qualifiers(qualifiers),
      fragment: encode_subpath(subpath)
    }
  end

  @spec encode_uri_path(
          type :: Purl.type(),
          namespace :: Purl.namespace(),
          name :: Purl.name(),
          version :: Purl.version() | nil
        ) :: iodata()
  defp encode_uri_path(type, namespace, name, version)

  defp encode_uri_path(type, namespace, name, %Version{} = version),
    do: encode_uri_path(type, namespace, name, Version.to_string(version))

  defp encode_uri_path(type, namespace, name, nil),
    do: Enum.intersperse([type | encode_namespace(namespace)] ++ [encode_name(name)], ?/)

  defp encode_uri_path(type, namespace, name, version),
    do: [encode_uri_path(type, namespace, name, nil), ?@, encode_version(version)]

  @spec encode_namespace(namespace :: Purl.namespace()) :: [String.t()]
  defp encode_namespace(namespace) do
    Enum.map(namespace, fn namespace_segment ->
      encode(namespace_segment)
    end)
  end

  @spec encode_name(name :: Purl.name()) :: String.t()
  defp encode_name(name), do: encode(name, [?/])

  @spec encode_qualifiers(qualifiers :: Purl.qualifiers()) :: String.t() | nil
  defp encode_qualifiers(qualifiers)
  defp encode_qualifiers(qualifiers) when qualifiers == %{}, do: nil

  defp encode_qualifiers(qualifiers) do
    qualifiers
    |> Enum.map_intersperse(?&, fn {key, value} ->
      [key, ?=, encode(value, [?&, ?+])]
    end)
    |> IO.iodata_to_binary()
  end

  @spec encode_version(version :: String.t()) :: String.t()
  defp encode_version(version), do: encode(version, [?/])

  @spec encode_subpath(subpath :: Purl.subpath()) :: String.t() | nil
  defp encode_subpath(subpath)
  defp encode_subpath([]), do: nil
  defp encode_subpath(subpath), do: subpath |> Enum.map_intersperse(?/, &encode/1) |> IO.iodata_to_binary()

  @spec encode(data :: String.t(), additional_codepoints :: charlist()) :: String.t()
  defp encode(data, additional_codepoints \\ []),
    do: URI.encode(data, &(&1 not in [?#, ??, ?@, ?[, ?] | additional_codepoints] and URI.char_unescaped?(&1)))
end
