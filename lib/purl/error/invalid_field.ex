defmodule Purl.Error.InvalidField do
  @moduledoc """
  Error raised in field values are invalid
  """

  @type t :: %__MODULE__{
          field: atom(),
          value: String.t()
        }

  defexception [:field, :value]

  @impl Exception
  def message(error)

  def message(%__MODULE__{field: field, value: value}) do
    formatted_value = inspect(value, pretty: true)
    "invalid field #{field}, #{formatted_value} given"
  end
end
