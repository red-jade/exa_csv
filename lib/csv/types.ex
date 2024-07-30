defmodule Exa.Csv.Types do
  @moduledoc "Types for CSV format."
  alias Exa.Types, as: E

  @typedoc "A column name as an atom or String."
  @type column() :: E.name()

  @typedoc "A list of column keys."
  @type columns() :: [columns()]

  @typedoc "A 0-based integer index for column numbers."
  @type index() :: E.index0()

  @typedoc "A key for columns: atom, String or 0-based integer index."
  @type key() :: index() | column()

  @typedoc "A non-empty list of keys for parser map or output record."
  @type keys() :: [key(), ...]

  @typedoc """
  A value can be:
  - `nil`
  - `true` or `false`
  - enum values (atom)
  - number (integer or float) 
  - text field (String) 
  - date-times (DateTime, Date or Time)
  - any custom type from a custom parser (e.g. URI, lat/lon location)
  """
  @type value() :: any()

  @typedoc "A non-empty list of values in a record."
  @type values() :: [value(), ...]

  @typedoc "A row of a CSV is a list, keyword list, indexed map or struct."
  @type record() :: values() | Keyword.t(value()) | %{key() => value()}

  @typedoc "A CSV document is a list of records."
  @type records() :: [record()]

  @typedoc "The result type for reading a CSV file."
  @type read_csv() :: {:csv, keys(), records()}
end
