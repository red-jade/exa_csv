defmodule Exa.Csv.Types do
  @moduledoc "Types for CSV format."

  @typedoc "The result type for reading a CSV file."
  @type read_csv() :: {:csv, records(), keys()}

  @typedoc "A row of a CSV is a list, keyword list, indexed map or struct."
  @type record() :: [value()] | Keyword.t() | %{key() => value()}

  @typedoc "A CSV document is a list of records."
  @type records() :: [record()]

  @typedoc "A column name as an atom."
  @type column() :: atom()

  @typedoc "A list of column keys."
  @type columns() :: [atom()]

  @typedoc "A 0-based integer index for column numbers."
  @type index() :: non_neg_integer()

  @typedoc "A key for columns: atom name or 0-based integer index."
  @type key() :: index() | column()

  @typedoc "The keys input option and resolved set of keys."
  @type keys() :: :list | :index | [index()] | columns()

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
