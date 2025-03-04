defmodule Purl.SpecialCase do
  @moduledoc false

  alias Purl.Error.SpecialCaseFailed

  @rules %{
    "bitbucket" => %{
      name_case_sensitive: false,
      name_normalize: :hyphen_case,
      namespace_case_sensitive: false
    },
    "cran" => %{
      version_required: true
    },
    "composer" => %{
      name_case_sensitive: false,
      namespace_case_sensitive: false
    },
    "github" => %{
      name_case_sensitive: false,
      namespace_case_sensitive: false
    },
    "huggingface" => %{
      version_case_sensitive: false
    },
    "pypi" => %{
      name_case_sensitive: false,
      name_normalize: :hyphen_case
    },
    "swift" => %{
      version_required: true,
      namespace_min_length: 1
    }
  }

  @spec apply(purl :: Purl.t()) ::
          {:ok, Purl.t()} | {:error, SpecialCaseFailed.t()}
  def apply(purl) do
    with {:ok, purl} <- downcase_namespace(purl),
         {:ok, purl} <- enforce_namespace_length(purl),
         {:ok, purl} <- downcase_name(purl),
         {:ok, purl} <- normalize_name(purl),
         {:ok, purl} <- downcase_version(purl),
         {:ok, purl} <- version_required(purl),
         {:ok, purl} <- conan_enforce_channel(purl) do
      cpan_verify_distribution(purl)
    end
  end

  @spec downcase_namespace(purl :: Purl.t()) :: {:ok, Purl.t()}
  defp downcase_namespace(purl)

  for {type, %{namespace_case_sensitive: false}} <- @rules do
    defp downcase_namespace(%Purl{type: unquote(type), namespace: namespace} = purl),
      do: {:ok, %{purl | namespace: Enum.map(namespace, &String.downcase/1)}}
  end

  defp downcase_namespace(purl), do: {:ok, purl}

  @spec enforce_namespace_length(purl :: Purl.t()) ::
          {:ok, Purl.t()} | {:error, SpecialCaseFailed.t()}
  defp enforce_namespace_length(purl)

  for {type, %{namespace_min_length: min_length}} <- @rules do
    defp enforce_namespace_length(%Purl{type: unquote(type), namespace: namespace} = purl) do
      if length(namespace) >= unquote(min_length) do
        {:ok, purl}
      else
        {:error, %SpecialCaseFailed{message: "namespace missing"}}
      end
    end
  end

  defp enforce_namespace_length(purl), do: {:ok, purl}

  @spec downcase_name(purl :: Purl.t()) :: {:ok, Purl.t()}
  defp downcase_name(purl)

  for {type, %{name_case_sensitive: false}} <- @rules do
    defp downcase_name(%Purl{type: unquote(type), name: name} = purl), do: {:ok, %{purl | name: String.downcase(name)}}
  end

  defp downcase_name(%Purl{type: "mlflow", name: name, qualifiers: %{"repository_url" => repository_url}} = purl) do
    with {:ok, %URI{host: host}} <- URI.new(repository_url),
         true <- String.ends_with?(host, "azuredatabricks.net") do
      {:ok, %{purl | name: String.downcase(name)}}
    else
      _other -> {:ok, purl}
    end
  end

  defp downcase_name(purl), do: {:ok, purl}

  @spec normalize_name(purl :: Purl.t()) :: {:ok, Purl.t()}
  defp normalize_name(purl)

  for {type, %{name_normalize: :hyphen_case}} <- @rules do
    defp normalize_name(%Purl{type: unquote(type), name: name} = purl),
      do: {:ok, %{purl | name: String.replace(name, "_", "-")}}
  end

  defp normalize_name(purl), do: {:ok, purl}

  @spec downcase_version(purl :: Purl.t()) :: {:ok, Purl.t()}
  defp downcase_version(purl)

  for {type, %{version_case_sensitive: false}} <- @rules do
    defp downcase_version(%Purl{type: unquote(type), version: version} = purl) when is_binary(version),
      do: {:ok, %{purl | version: String.downcase(version)}}

    defp downcase_version(%Purl{type: unquote(type), version: %Version{} = version} = purl),
      do: {:ok, %{purl | version: version |> Version.to_string() |> String.downcase() |> Version.parse!()}}
  end

  defp downcase_version(purl), do: {:ok, purl}

  @spec version_required(purl :: Purl.t()) ::
          {:ok, Purl.t()} | {:error, SpecialCaseFailed.t()}
  defp version_required(purl)

  for {type, %{version_required: true}} <- @rules do
    defp version_required(%Purl{type: unquote(type), version: nil} = _purl),
      do: {:error, %SpecialCaseFailed{message: "version missing"}}
  end

  defp version_required(purl), do: {:ok, purl}

  @spec conan_enforce_channel(purl :: Purl.t()) ::
          {:ok, Purl.t()} | {:error, SpecialCaseFailed.t()}
  defp conan_enforce_channel(purl)

  defp conan_enforce_channel(%Purl{type: "conan", namespace: [_one | _rest], qualifiers: qualifiers} = purl)
       when is_map_key(qualifiers, "channel"),
       do: {:ok, purl}

  defp conan_enforce_channel(%Purl{type: "conan", namespace: [], qualifiers: qualifiers} = purl)
       when not is_map_key(qualifiers, "channel"),
       do: {:ok, purl}

  defp conan_enforce_channel(%Purl{type: "conan"}),
    do: {:error, %SpecialCaseFailed{message: "either namespace & channel must both be present or both be absent"}}

  defp conan_enforce_channel(purl), do: {:ok, purl}

  @spec cpan_verify_distribution(purl :: Purl.t()) ::
          {:ok, Purl.t()} | {:error, SpecialCaseFailed.t()}
  defp cpan_verify_distribution(purl)

  defp cpan_verify_distribution(%Purl{type: "cpan", namespace: [], name: name} = purl) do
    if String.contains?(name, "-") do
      {:error,
       %SpecialCaseFailed{
         message: "cpan modules must not contain \"-\""
       }}
    else
      {:ok, purl}
    end
  end

  defp cpan_verify_distribution(%Purl{type: "cpan", namespace: [_one | _rest] = namespace, name: name} = purl) do
    if String.contains?(name, "::") do
      {:error,
       %SpecialCaseFailed{
         message: "cpan distribution name must not contain \"::\""
       }}
    else
      {:ok, %{purl | namespace: Enum.map(namespace, &String.upcase/1)}}
    end
  end

  defp cpan_verify_distribution(purl), do: {:ok, purl}
end
