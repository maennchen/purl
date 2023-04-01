defmodule Purl.Error.SpecialCaseFailed do
  @moduledoc """
  Error raised if special rules for type are not fulfilled
  """

  @type t :: %__MODULE__{
          message: String.t()
        }

  defexception [:message]
end
