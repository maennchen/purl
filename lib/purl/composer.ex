defmodule Purl.Composer do
  @moduledoc false

  @spec compose_uri(purl :: Purl.t()) :: URI.t()
  def compose_uri(purl)

  def compose_uri(%Purl{version: %Version{} = version} = purl),
    do: compose_uri(%Purl{purl | version: Version.to_string(version)})

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
      path:
        Enum.join(
          [type | encode_namespace(namespace)] ++
            [
              case version do
                nil -> encode_name(name)
                version -> "#{encode_name(name)}@#{encode_version(version)}"
              end
            ],
          "/"
        ),
      query:
        unless qualifiers == %{} do
          encode_qualifiers(qualifiers)
        end,
      fragment:
        unless subpath == [] do
          Enum.join(subpath, "/")
        end
    }
  end

  @spec encode_namespace(namespace :: Purl.namespace()) :: [String.t()]
  defp encode_namespace(namespace) do
    Enum.map(namespace, fn namespace_segment ->
      URI.encode(namespace_segment, &(&1 != ?@ and URI.char_unescaped?(&1)))
    end)
  end

  @spec encode_name(name :: Purl.name()) :: [String.t()]
  defp encode_name(name) do
    URI.encode(name, &(&1 != ?@ and URI.char_unescaped?(&1)))
  end

  @spec encode_qualifiers(qualifiers :: Purl.qualifiers()) :: String.t()
  defp encode_qualifiers(qualifiers) do
    Enum.map_join(qualifiers, "&", fn {key, value} ->
      URI.encode(key, &URI.char_unreserved?/1) <>
        "=" <> URI.encode(value, &(&1 == ?/ or URI.char_unescaped?(&1)))
    end)
  end

  @spec encode_version(version :: String.t()) :: String.t()
  defp encode_version(version), do: URI.encode(version, &URI.char_unreserved?/1)
end
