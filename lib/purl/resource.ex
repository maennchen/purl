defmodule Purl.Resource do
  @moduledoc false

  @spec from_resource_uri(uri :: String.t() | URI.t()) :: {:ok, Purl.t()} | :error
  def from_resource_uri(uri)

  def from_resource_uri(uri) when is_binary(uri) do
    uri
    |> URI.new!()
    |> from_resource_uri()
  rescue
    e in URI.Error ->
      try do
        uri
        |> convert_git_to_normal_uri()
        |> URI.new!()
        |> from_resource_uri()
      rescue
        URI.Error -> reraise e, __STACKTRACE__
      end
  end

  def from_resource_uri(%URI{
        scheme: "https",
        host: "github.com",
        port: 443,
        path: path,
        query: nil,
        fragment: nil
      }) do
    path = path |> String.trim_trailing(".git") |> String.trim("/")

    case String.split(path, "/") do
      [user, repository] -> Purl.new(%Purl{type: "github", namespace: [user], name: repository})
      _other -> :error
    end
  end

  def from_resource_uri(%URI{
        scheme: "git+ssh",
        userinfo: "git",
        host: "github.com",
        path: path
      }) do
    path = path |> String.trim_trailing(".git") |> String.trim("/")

    case String.split(path, "/") do
      [user, repository] -> Purl.new(%Purl{type: "github", namespace: [user], name: repository})
      _other -> :error
    end
  end

  def from_resource_uri(%URI{
        scheme: "https",
        host: "bitbucket.org",
        port: 443,
        path: path,
        query: nil,
        fragment: nil
      }) do
    path = path |> String.trim_trailing(".git") |> String.trim("/")

    case String.split(path, "/") do
      [user, repository] ->
        Purl.new(%Purl{type: "bitbucket", namespace: [user], name: repository})

      _other ->
        :error
    end
  end

  def from_resource_uri(%URI{
        scheme: "git+ssh",
        userinfo: "git",
        host: "bitbucket.org",
        path: path
      }) do
    path = path |> String.trim_trailing(".git") |> String.trim("/")

    case String.split(path, "/") do
      [user, repository] ->
        Purl.new(%Purl{type: "bitbucket", namespace: [user], name: repository})

      _other ->
        :error
    end
  end

  def from_resource_uri(%URI{
        scheme: "https",
        host: "hex.pm",
        port: 443,
        path: "/packages/" <> package,
        query: nil,
        fragment: nil
      }) do
    path = String.trim(package, "/")

    case String.split(path, "/", parts: 2) do
      [package | _rest] -> Purl.new(%Purl{type: "hex", namespace: [], name: package})
      _other -> :error
    end
  end

  def from_resource_uri(_unknown), do: :error

  @spec convert_git_to_normal_uri(String.t()) :: String.t()
  defp convert_git_to_normal_uri(uri) do
    "git+ssh://" <> String.replace(uri, ":", "/", global: false)
  end
end
