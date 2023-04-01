defmodule Purl.Error.InvalidScheme do
  @moduledoc """
  Error raised if the scheme of the purl is incorrect
  """

  @type t :: %__MODULE__{
          scheme: String.t() | nil
        }

  defexception [:scheme]

  @impl Exception
  def message(error)

  def message(%__MODULE__{scheme: scheme}) do
    formatted_scheme = inspect(scheme, pretty: true)
    "scheme #{formatted_scheme} is invalid, pkg expected"
  end
end
