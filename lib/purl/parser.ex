defmodule Purl.Parser do
  @moduledoc false

  @spec parse(purl :: String.t() | URI.t() | Purl.t()) ::
          {:ok, Purl.t()} | {:error, Purl.parse_error()}
  def parse(purl)

  def parse(string) when is_binary(string) do
    case URI.new(string) do
      {:ok, uri} ->
        parse(uri)

      {:error, invalid} ->
        {:error, %URI.Error{action: :parse, reason: :invalid_uri, part: invalid}}
    end
  end

  def parse(%URI{scheme: nil} = _uri), do: {:error, %Purl.Error.InvalidScheme{scheme: nil}}

  def parse(%URI{scheme: scheme} = _uri) when scheme != "pkg",
    do: {:error, %Purl.Error.InvalidScheme{scheme: scheme}}

  def parse(%URI{scheme: "pkg", authority: type, path: path} = uri) when type != nil,
    do: parse(%URI{uri | authority: nil, path: type <> "/" <> path})

  def parse(%URI{scheme: "pkg", host: type, path: path} = uri) when type != nil,
    do: parse(%URI{uri | host: nil, path: type <> "/" <> path})

  def parse(
        %URI{
          scheme: "pkg",
          authority: nil,
          host: nil,
          path: path,
          fragment: fragment,
          query: query
        } = _uri
      ) do
    with {:ok, {type, namespace, name, version}} <- parse_path(path),
         {:ok, qualifiers} <- parse_query(query) do
      parse(%Purl{
        type: type,
        namespace: namespace,
        name: name,
        version: version,
        qualifiers: qualifiers,
        subpath:
          case fragment do
            nil -> []
            fragment -> fragment |> URI.decode() |> String.trim("/") |> String.split("/")
          end
      })
    end
  end

  def parse(
        %Purl{
          type: type,
          namespace: namespace,
          name: name,
          version: version,
          qualifiers: qualifiers,
          subpath: subpath
        } = _purl
      ) do
    with {:ok, type} <- parse_type(type),
         {:ok, namespace} <- parse_namespace(namespace),
         {:ok, name} <- parse_name(name),
         {:ok, version} <- parse_version(version),
         {:ok, qualifiers} <- parse_qualifiers(qualifiers),
         {:ok, subpath} <- parse_subpath(subpath) do
      {:ok,
       %Purl{
         type: type,
         namespace: namespace,
         name: name,
         version: version,
         qualifiers: qualifiers,
         subpath: subpath
       }}
    end
  end

  @spec parse_path(path :: String.t()) ::
          {:ok, {String.t(), [String.t()], String.t(), String.t() | nil}}
          | {:error, Purl.parse_error()}
  defp parse_path(path) do
    case String.split(path, "/", trim: true) do
      [type | [_name_or_namespace | _rest] = rest] ->
        type = String.downcase(type)

        {name, namespace} = List.pop_at(rest, -1)

        namespace = Enum.map(namespace, &URI.decode/1)

        {name, version} =
          case String.split(name, "@", parts: 2) do
            [name, version] -> {name, URI.decode(version)}
            [name] -> {name, nil}
          end

        {:ok, {type, namespace, name, version}}

      [_one] ->
        {:error, %Purl.Error.InvalidField{field: :name, value: ""}}
    end
  end

  @spec parse_query(query :: String.t() | nil) ::
          {:ok, %{optional(String.t()) => String.t()}} | {:error, Purl.parse_error()}
  defp parse_query(query)
  defp parse_query(nil), do: {:ok, %{}}

  defp parse_query(query) do
    query
    |> URI.query_decoder()
    |> Enum.reduce_while({:ok, %{}}, fn {key, value}, {:ok, acc} ->
      key = String.downcase(key)

      if Map.has_key?(acc, key) do
        {:halt, {:error, %Purl.Error.DuplicateQualifier{key: key}}}
      else
        {:cont, {:ok, Map.put(acc, key, value)}}
      end
    end)
  end

  @spec parse_type(type :: String.t()) ::
          {:ok, Purl.type()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_type(type) when is_binary(type) do
    if Regex.match?(~r/^[a-zA-Z\.\+\-][a-zA-Z0-9\.\+\-]+$/, type) do
      {:ok, type}
    else
      {:error, %Purl.Error.InvalidField{field: :type, value: type}}
    end
  end

  @spec parse_namespace(namespace :: [String.t()]) ::
          {:ok, Purl.namespace()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_namespace(namespace) when is_list(namespace) do
    namespace
    |> Enum.reduce_while({:ok, []}, fn
      segment, {:ok, acc} ->
        case parse_namespace_segment(segment) do
          {:ok, segment} -> {:cont, {:ok, [segment | acc]}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
    end)
    |> case do
      {:ok, namespace} -> {:ok, Enum.reverse(namespace)}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec parse_namespace_segment(segment :: String.t()) ::
          {:ok, Purl.namespace_segment()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_namespace_segment(segment)

  defp parse_namespace_segment(""),
    do: {:error, %Purl.Error.InvalidField{field: :namespace, value: ""}}

  defp parse_namespace_segment(segment) do
    cond do
      !String.valid?(segment) ->
        {:error, %Purl.Error.InvalidField{field: :namespace, value: segment}}

      String.contains?(segment, "/") ->
        {:error, %Purl.Error.InvalidField{field: :namespace, value: segment}}

      true ->
        {:ok, segment}
    end
  end

  @spec parse_name(name :: String.t()) ::
          {:ok, Purl.name()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_name(name)
  defp parse_name(""), do: {:error, %Purl.Error.InvalidField{field: :name, value: ""}}

  defp parse_name(name) when is_binary(name) do
    if String.valid?(name) do
      {:ok, name}
    else
      {:error, %Purl.Error.InvalidField{field: :name, value: name}}
    end
  end

  @spec parse_version(version :: Version.t() | String.t() | nil) ::
          {:ok, Purl.version() | nil} | {:error, Purl.Error.InvalidField.t()}
  defp parse_version(version)
  defp parse_version(nil), do: {:ok, nil}
  defp parse_version(%Version{} = version), do: {:ok, version}

  defp parse_version(version) when is_binary(version) do
    if String.valid?(version) do
      {:ok, version}
    else
      {:error, %Purl.Error.InvalidField{field: :version, value: version}}
    end
  end

  @spec parse_qualifiers(qualifiers :: %{optional(String.t()) => String.t()}) ::
          {:ok, Purl.qualifiers()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_qualifiers(%{} = qualifiers) do
    Enum.reduce_while(qualifiers, {:ok, %{}}, fn {qualifier_key, qualifier_value}, {:ok, acc} ->
      with {:ok, qualifier_key} <- parse_qualifier_key(qualifier_key),
           {:ok, qualifier_value} <- parse_qualifier_value(qualifier_value) do
        {:cont, {:ok, Map.put(acc, qualifier_key, qualifier_value)}}
      else
        error -> {:halt, error}
      end
    end)
  end

  @spec parse_qualifier_key(qualifier_key :: String.t()) ::
          {:ok, Purl.qualifier_key()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_qualifier_key(qualifier_key)

  defp parse_qualifier_key(""),
    do: {:error, %Purl.Error.InvalidField{field: :qualifiers, value: ""}}

  defp parse_qualifier_key(qualifier_key) do
    if Regex.match?(~r/^[a-zA-Z\.\-\_][a-zA-Z0-9\.\-\_]+$/, qualifier_key) do
      {:ok, qualifier_key}
    else
      {:error, %Purl.Error.InvalidField{field: :qualifiers, value: qualifier_key}}
    end
  end

  @spec parse_qualifier_value(qualifier_value :: String.t()) ::
          {:ok, Purl.qualifier_value()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_qualifier_value(qualifier_value)

  defp parse_qualifier_value(""),
    do: {:error, %Purl.Error.InvalidField{field: :qualifiers, value: ""}}

  defp parse_qualifier_value(qualifier_value) do
    if String.valid?(qualifier_value) do
      {:ok, qualifier_value}
    else
      {:error, %Purl.Error.InvalidField{field: :qualifiers, value: qualifier_value}}
    end
  end

  @spec parse_subpath(subpath :: [String.t()]) ::
          {:ok, Purl.subpath()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_subpath(subpath) when is_list(subpath) do
    subpath
    |> Enum.reduce_while({:ok, []}, fn
      segment, {:ok, acc} ->
        case parse_subpath_segment(segment) do
          :skip -> {:cont, {:ok, acc}}
          {:ok, segment} -> {:cont, {:ok, [segment | acc]}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
    end)
    |> case do
      {:ok, subpath} -> {:ok, Enum.reverse(subpath)}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec parse_subpath_segment(segment :: String.t()) ::
          {:ok, Purl.subpath_segment()} | {:error, Purl.Error.InvalidField.t()}
  defp parse_subpath_segment(segment)

  defp parse_subpath_segment(segment) when segment in ["", ".", ".."], do: :skip

  defp parse_subpath_segment(segment) when is_binary(segment) do
    if String.valid?(segment) do
      {:ok, segment}
    else
      {:error, %Purl.Error.InvalidField{field: :subpath, value: segment}}
    end
  end
end
