defmodule Exa.Csv.CsvWriter do
  @moduledoc "Utilities to write CSV format."

  require Logger
  use Exa.Csv.Constants

  alias Exa.Text, as: T

  import Exa.Indent, except: [row: 4]
  alias Exa.Indent, as: I

  import Exa.Types
  alias Exa.Types, as: E

  import Exa.Csv.Types
  alias Exa.Csv.Types, as: C

  alias Exa.Option

  # document ----------

  @doc "Write CSV data to file."
  @spec to_file(C.records(), E.options(), String.t()) :: T.textdata() | {:error, any()}
  def to_file(recs, opts \\ [], filename) when is_string_nonempty(filename) do
    case encode(recs, opts) do
      {:error, _} = err -> err
      text -> Exa.File.to_file_text(text, filename)
    end
  rescue
    err -> {:error, err}
  end

  @doc """
  Create a new CSV document.

  The argument is a list of records.
  The records can be lists, keyword lists, or map/structs.
  The records must be homogeneous (all the same type).
  A map is unordered, so the column list 
  must be explicitly provided in the options.

  Column name strings are converted to atom keys by:
  trimming all whitespace, reducing internal runs of whitespace 
  to a single underscore `'_'`, removing all non-alphanumeric characters,
  and downcasing alphabetic characters.

  Options:
  - `:delim` the character delimiter between fields. 
    Default: comma `?,`, other common values include `?|` or `?\t`.

  - `:columns` the list of column names (strings or atoms).
    Column names are used to write the column name headers,
    and as keys to order the writing of column data for each row.
    Names are required for lists when `:header` is true.
    If columns are not provided for keyword and map/struct records,
    the keys will be extracted from the first record,
    and the default ordering will be used.

  - `:header` boolean flag to control writing a column header line.

  - `:null` the value (string or atom) written for `nil` values, 
     or `:empty` for no value to be written (default).

  - formatting functions for specific datatypes. 
    For example: booleans to be 0 or 1; 
    integers to hex, or with locale separators; 
    float to specific precision; 
    dates and times to locale-dependent strings:
    - `:atom`: default `to_string/1`, except `nil` (see above)
    - `:bool`: default `to_string/1`, giving `true` and `false`
    - `:integer`: default `to_string/1`
    - `:float`: default round to 4 decimal places
    - `:datetime`: datetimes, default ISO 8601
    - `:date`: date, default ISO 8601
    - `:time`: time, default ISO 8601

  The result is text data for the whole CSV document.
  """
  @spec encode(C.records(), E.options()) :: T.textdata() | {:error, any()}
  def encode(recs, opts) do
    omap = %{
      :delim => Option.get_char(opts, :delim, ?,),
      :null => Option.get_name(opts, :null, :empty),
      :afun => Option.get_fun(opts, :atom, &to_string/1),
      :bfun => Option.get_fun(opts, :bool, &to_string/1),
      :ifun => Option.get_fun(opts, :integer, &to_string/1),
      :ffun => Option.get_fun(opts, :float, &float_formatter/1),
      :dtfun => Option.get_fun(opts, :datetime, &DateTime.to_iso8601/1),
      :dfun => Option.get_fun(opts, :date, &Date.to_iso8601/1),
      :tfun => Option.get_fun(opts, :time, &Time.to_iso8601/1)
    }

    head = Option.get_bool(opts, :header, false)
    # TODO - fix for string/atom 
    cols = Option.get_list_nonempty_name(opts, :columns, [])

    rec1 = hd(recs)
    ncol = rec_length(rec1)

    if cols != [] and length(cols) != ncol do
      msg = "Column list must have same length as the data records"
      Logger.error(msg)
      raise ArgumentError, message: msg
    end

    io =
      if head do
        cols = if cols == [], do: columns(rec1), else: cols
        row(indent(), cols, omap)
      else
        indent()
      end

    # convert string names to atoms keys for keyword/map/struct access
    cols = Enum.map(cols, &Exa.String.to_atom/1)

    Enum.reduce(recs, io, fn rec, io ->
      if rec_length(rec) != ncol do
        msg = "Wrong number of columns, expecting #{ncol}, found #{rec_length(rec)}"
        Logger.error(msg)
        raise ArgumentError, message: msg
      end

      rec(io, rec, cols, omap)
    end)
  rescue
    err -> {:error, err}
  end

  @spec float_formatter(float(), E.count1()) :: String.t()
  defp float_formatter(x, dp \\ 4) do
    x |> Float.round(dp) |> Float.to_string()
  end

  @spec rec_length(C.record()) :: pos_integer()

  defp rec_length(rec) when is_list(rec), do: length(rec)
  defp rec_length(rec) when is_map(rec), do: map_size(rec)

  defp rec_length(rec) do
    msg = "Record must be list, keyword, map or struct, found #{rec}"
    Logger.error(msg)
    raise ArgumentError, message: msg
  end

  @dialyzer {:nowarn_function, columns: 1}
  @spec columns(C.record()) :: C.columns()

  defp columns(rec) when is_keyword(rec), do: Keyword.keys(rec)
  defp columns(rec) when is_struct(rec), do: Map.keys(rec) -- [:__struct__]
  defp columns(rec) when is_map(rec), do: Map.keys(rec)

  defp columns(rec) when is_list(rec) do
    msg = "Cannot get column headers from row list data"
    Logger.error(msg)
    raise ArgumentError, message: msg
  end

  # write a list of values from row data and an optional key list
  @spec rec(I.indent(), C.record(), C.columns(), map()) :: I.indent()

  defp rec(io, rec, [], omap) when is_keyword(rec) do
    row(io, Keyword.values(rec), omap)
  end

  defp rec(io, rec, cols, omap) when is_keyword(rec) do
    vals = Enum.map(cols, &Keyword.fetch!(rec, &1))
    row(io, vals, omap)
  end

  defp rec(io, rec, _, omap) when is_list(rec) do
    row(io, rec, omap)
  end

  defp rec(io, rec, [], omap) when is_map(rec) do
    row(io, Map.values(rec), omap)
  end

  defp rec(io, rec, cols, omap) when is_map(rec) do
    vals = Enum.map(cols, &Map.fetch!(rec, &1))
    row(io, vals, omap)
  end

  @spec row(I.indent(), [C.value()], map()) :: I.indent()
  defp row(io, [v1 | rest], omap) do
    io
    |> newl()
    |> val(v1, omap)
    |> reduce(rest, fn v, io -> io |> chr(omap.delim) |> val(v, omap) end)
    |> endl()
  end

  # TODO - custom formatters per-column

  @spec val(I.indent(), C.value(), map()) :: I.indent()
  defp val(io, nil, omap), do: null(io, omap.null)
  defp val(io, b, omap) when is_boolean(b), do: txt(io, omap.bfun.(b))
  defp val(io, a, omap) when is_atom(a), do: txt(io, omap.afun.(a))
  defp val(io, i, omap) when is_integer(i), do: txt(io, omap.ifun.(i))
  defp val(io, x, omap) when is_float(x), do: txt(io, omap.ffun.(x))
  defp val(io, s, omap) when is_string(s), do: string(io, s, omap)
  defp val(io, dt, omap) when is_struct(dt, DateTime), do: txt(io, omap.dtfun.(dt))
  defp val(io, d, omap) when is_struct(d, Date), do: txt(io, omap.dfun.(d))
  defp val(io, t, omap) when is_struct(t, Time), do: txt(io, omap.tfun.(t))

  @spec string(I.indent(), String.t(), map()) :: I.indent()
  defp string(io, str, delim) when is_string(str) do
    cs = String.to_charlist(str)

    if Enum.any?(cs, fn c -> c == delim or c in [?", ?\n, ?\r] end) do
      txt(io, [?", esc(cs, []), ?"])
    else
      txt(io, cs)
    end
  end

  @spec esc([char()], [char()]) :: [char()]
  defp esc([?" | rest], out), do: esc(rest, [?", ?" | out])
  defp esc([c | rest], out), do: esc(rest, [c | out])
  defp esc([], out), do: Enum.reverse(out)

  @spec null(I.indent(), :empty | String.t()) :: I.indent()
  defp null(io, :empty), do: io
  defp null(io, null), do: str(io, null)
end
