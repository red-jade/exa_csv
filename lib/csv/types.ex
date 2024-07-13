defmodule Exa.Csv.Types do
  @moduledoc "Types for CSV format."

  @typedoc "The result type for reading a CSV file."
  @type read_csv() :: {:csv, records(), :list | :index | columns()}

  @typedoc "A row of a CSV is a list, keyword list, indexed map, map or struct."
  @type record() :: [value()] | Keyword.t() | %{index() => value()} | %{atom() => value()}

  @typedoc "A CSV document is a list of records."
  @type records() :: [record()]

  @typedoc """
  When the records are maps or structs,
  the order of column keys may be specified.
  """
  @type columns() :: [atom()]

  @typedoc "A 0-based integer index for column numbers."
  @type index() :: non_neg_integer()

  @typedoc """
  A value can be:
  - `nil`
  - `true` or `false`
  - number (integer or float) 
  - text field (String or atom) 
  - date-times (DateTime, Date or Time)
  """
  @type value() ::
          nil | atom() | integer() | float() | String.t() | DateTime.t() | Date.t() | Time.t()
end
