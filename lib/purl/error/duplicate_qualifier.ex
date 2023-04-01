defmodule Purl.Error.DuplicateQualifier do
  @moduledoc """
  Error raised if qualifier keys are duplicated
  """

  @type t :: %__MODULE__{
          key: String.t()
        }

  defexception [:key]

  @impl Exception
  def message(error)

  def message(%__MODULE__{key: key}) do
    "qualifier #{key} is duplicated"
  end
end
