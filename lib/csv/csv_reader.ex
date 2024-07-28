defmodule Exa.Csv.CsvReader do
  @moduledoc "Utilities to parse CSV format."

  require Logger
  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Parse, as: P

  import Exa.Csv.Types
  alias Exa.Csv.Types, as: C

  alias Exa.Option
  alias Exa.Parse

  @nulls ["", "null", "nil", "nan", "inf"]

  @doc """
  Read a CSV file.

  The file is assumed to be encoded in UTF-8 (UTF16/UTF32 LE/BE not supported).
  If the file has a UTF-8 BOM, it will be ignored.

  See `decode` for descritption of the `:options`.
  """
  @dialyzer {:nowarn_function, from_file: 2}
  @spec from_file(String.t(), E.options()) :: C.read_csv() | {:error, any()}
  def from_file(filename, opts \\ []) when is_nonempty_string(filename) do
    case Exa.File.from_file_text(filename) do
      {:error, _} = err -> err
      text -> text |> Exa.File.bom!() |> decode(opts)
    end
  rescue
    err -> {:error, err}
  end

  @doc """
  Decode CSV text.

  The result is a list of row records
  and an optional list of column names (atoms).

  If the CSV file has a header line, 
  and the `:header` option is `true`
  then column names are read from the file.

  If the CSV file does not have a header line, 
  the column names can be explicitly provided 
  in the `:columns` option (list of strings or atoms).

  If column names are not available in either header or options, 
  then the last element of the result is `nil`.

  If the `:index` flag is `true` then records will be returned 
  as Maps indexed by 0-based integers for the column number,
  and the last element of the result will be an integer sequence.
  Note that `index: true` overrides the `:object` factory.
  Otherwise, if there are no column names (hence no object factory),
  and `:index` is false, then records are returned as lists,
  and the last element of the result will be `:list`.

  If column names are available, from header or option, 
  then they are: 
  - used to look-up parser functions
  - used as a set of keys to build row records
  - returned as the last element in the result
  Column names must be unique.

  Column name strings are converted to atom keys by:
  trimming all whitespace, reducing internal runs of whitespace 
  to a single underscore `'_'`, removing all non-alphanumeric characters,
  and downcasing alphabetic characters.

  If column keys are available from column names or `index: true`,
  then individual column parsers can be specified in the `:parsers` option.
  The parser map is keyed by column key (atom or 0-based integer).
  One or more parsers can be provided, not every column must have a parser.
  The parser value must be a function `(String.t() -> xyz())`.

  If no custom parsers are specified, 
  then a default parser can be provided using the `:pardef` option.

  The default default parser is `Exa.Parse.guess/4`.
  It will convert `nil` values, then identify booleans, 
  integers, floats, DateTimes, Dates and Times.
  For temporal types, the ISO8601 formats are assumed.
  If any error occurs in a parser, the value is given as
  `{:error, string_cell_value}`.

  The `:object` options determines how rows are returned to Elixir.
  The option value should be a factory function, 
  or the (atom) name of a module that has a `new` method 
  to be used as a factory.

  The factory method (or `new`) takes a list of `{key,value}` 
  pairs to create a new object. 
  For example, `Keyword` (default) or `Map` both have the required `new` function.
  The `Exa.Factory` can generate a factory function from a list of structs.
  A custom module could create application-specific objects
  based on the set of keys in the list.

  Options:
  - `:delim` the character delimiter between fields. 
    Default: comma `?,`, other common values include `?|` or `?\\t`.

  - `:header` boolean flag to control reading of optional column header line.
    Column names must be unique.

  - `:columns` the list of column names (strings or atoms).
    Column names are used: to look-up parsers to read data;
    as keys to build data objects for each row;
    and returned as the last element of the result. 
    Column names must be unique.

  - `:index` boolean flag to force use of 0-based sequential integers
    as the keys for building output data records as Keyword lists.
    This option is useful for files that have no header line,
    and no previously known list of column names.
    If `true` it overrides any `:object` factory method,
    and all records will be returned as Maps with 0-based integer keys.

  - `:parsers` a map of parser functions for specific columns. 
    The key is a column name converted to an atom,
    or a 0-based index of the column number.
    The parser function must be `(nil | String.t() -> nil | String.t() | xyz())`.
    A `nil` argument should be passed through as a `nil` result.
    The function should not raise errors. 
    Any errors will be caught and replaced with the value `{:error, input}`.

  - `:pardef` a default parser that will be invoked 
    if there is no specific parser for the column.
    The parser function must be `(String.t() -> nil | String.t() | any())`.

  - `:nulls` a list of case-insensitive string values 
    that will be converted to `nil` by the 
    default default parser (`Exa.Parse.guess/4`).
    The default values are: `#{@nulls}`.
    The default default parser will only be invoked when there 
    is no custom column-specific parser, 
    and no generic default parser specified in the options.
  """
  @spec decode(String.t(), E.options()) :: C.read_csv()
  def decode(csv, opts \\ []) when is_string(csv) do
    delim = Option.get_char(opts, :delim, ?,)
    nulls = Option.get_list_string(opts, :nulls, [])
    nulls = if nulls == [], do: @nulls, else: Enum.map(nulls, &String.downcase/1)
    pardef = Option.get_fun(opts, :pardef, Parse.guess(nulls))
    parsers = Option.get_map(opts, :parsers, %{})

    # index overrides factory
    index = Option.get_bool(opts, :index, false)
    fac = Keyword.get(opts, :object, nil)

    # facfun is map, :list, or factory function
    facfun =
      cond do
        index ->
          # index true means prepare a map of parsers
          &Map.new/1

        is_nil(fac) ->
          # false index and no factory function, so just list
          :list

        is_function(fac, 1) ->
          # explicit factory function
          fac

        is_module(fac) and function_exported?(fac, :new, 1) ->
          # factory function is module with 'new' function
          &fac.new/1

        true ->
          msg = "Illegal ':object' option '#{fac}'"
          Logger.error(msg)
          raise ArgumentError, message: msg
      end

    # TODO - change header to be a nonneg int count of rows to ignore
    head = Option.get_bool(opts, :header, false)
    cols = Option.get_list_nonempty_name(opts, :columns, [])

    {headers, csv} =
      if head do
        vals(delim, csv, [])
      else
        {cols, csv}
      end

    if cols != [] and headers != [] do
      # TODO - compare cols options with actual header strings?
      if length(cols) != length(headers) do
        msg = "Column header mismatch, expected #{cols}, found #{headers}"
        Logger.error(msg)
        raise ArgumentError, message: msg
      end
    end

    nheader = length(headers)

    # keys is :index, :list, list of header atoms, or list of integers
    # convert string names to atoms keys for keyword/map/struct access
    # index overrides column names
    # if we know the number of columns, generate the index keys
    # otherwise tag :index and use length of the first row
    keys =
      cond do
        index and nheader > 0 ->
          1..nheader

        index ->
          :index

        headers != [] ->
          Enum.map(headers, fn
            h when is_atom(h) -> h
            h when is_string(h) -> h |> String.downcase() |> Exa.String.to_atom()
          end)

        true ->
          :list
      end

    omap = %{
      :delim => delim,
      :facfun => facfun,
      :keys => keys,
      :parsers => parsers,
      :pardef => pardef
    }

    rows(csv, omap, [])
  end

  # ------
  # parser 
  # ------

  # read a list of objects from the rows of the csv up to EOF
  @spec rows(String.t(), map(), C.records()) :: C.read_csv()

  defp rows(<<>>, omap, rows), do: {:csv, Enum.reverse(rows), omap.keys}

  defp rows(csv, omap, rows) do
    {vals, rest} = vals(omap.delim, csv, [])

    # use first row to set the number of columns
    omap =
      if omap.keys == :index do
        %{omap | :keys => Range.to_list(0..(length(vals) - 1))}
      else
        omap
      end

    if omap.keys != :list and length(omap.keys) != length(vals) do
      msg =
        "Mismatch number of columns, " <>
          "expect #{length(omap.keys)}, found #{length(vals)}"

      Logger.info("Keys #{inspect(omap.keys, limit: :infinity)}")
      Logger.info("Vals #{inspect(vals, limit: :infinity)}")
      Logger.error(msg)
      raise ArgumentError, message: msg
    end

    obj =
      vals
      |> parse(omap.keys, omap.pardef, omap.parsers, [])
      |> build(omap.keys, omap.facfun)

    rows(rest, omap, [obj | rows])
  end

  # parse the values for nil and custom types
  @spec parse([C.value()], C.keys(), P.parfun(any()), %{C.key() => P.parfun(any())}, [any()]) ::
          [any()]

  defp parse([], _, _, _, pvals), do: Enum.reverse(pvals)

  defp parse([v | vals], :list, pardef, parsers, pvals) do
    pv = do_parse(v, nil, pardef)
    parse(vals, :list, pardef, parsers, [pv | pvals])
  end

  defp parse([v | vals], [k | keys], pardef, parsers, pvals) do
    kpar = Map.get(parsers, k, nil)
    pv = do_parse(v, kpar, pardef)
    parse(vals, keys, pardef, parsers, [pv | pvals])
  end

  @spec do_parse(C.value(), nil | P.parfun(any()), P.parfun(any())) :: any()
  defp do_parse(v, kpar, pardef) do
    if is_nil(kpar), do: pardef.(v), else: kpar.(v)
  rescue
    err ->
      Logger.error("Parser error: #{inspect(err)}")
      {:error, v}
  end

  # read a list of values in one row of the csv up to EOL or EOF
  @spec vals(char(), String.t(), [C.value()]) :: {[C.value()], binary()}

  # subsequent columns have a leading delimiter
  defp vals(d, <<d::utf8, ?", rest::binary>>, vals) do
    {v, rest} = quoted(d, rest, <<>>)
    vals(d, rest, [v | vals])
  end

  # first column does not have a leading delimiter
  defp vals(d, <<?", rest::binary>>, vals) do
    {v, rest} = quoted(d, rest, <<>>)
    vals(d, rest, [v | vals])
  end

  defp vals(_, <<?\n, rest::binary>>, vals), do: {Enum.reverse(vals), rest}

  defp vals(_, <<>>, vals), do: {Enum.reverse(vals), <<>>}

  # subsequent columns have a leading delimiter
  defp vals(d, <<d::utf8, rest::binary>>, vals) do
    {v, rest} = raw(d, rest, <<>>)
    vals(d, rest, [v | vals])
  end

  # first column does not have a leading delimiter
  defp vals(d, <<rest::binary>>, vals) do
    {v, rest} = raw(d, rest, <<>>)
    vals(d, rest, [v | vals])
  end

  # build objects from the specified keys and list of values from the row
  @spec build([C.value()], :list | [atom()], :list | Exa.Factory.factory_fun()) :: any()
  defp build(vals, _, :list), do: vals
  defp build(vals, keys, facfun), do: keys |> Enum.zip(vals) |> facfun.()

  # TODO - ignore whitespace before and after quoted value

  # read a quoted string up to the close quotes
  # consume any following delimiter, but not EOL or EOF
  @spec quoted(char(), String.t(), String.t()) :: {String.t(), String.t()}
  defp quoted(d, <<?", ?", rest::binary>>, str), do: quoted(d, rest, <<str::binary, ?">>)
  defp quoted(d, <<?", d::utf8, rest::binary>>, str), do: {str, rest}
  defp quoted(_, <<?", rest::binary>>, str), do: {str, rest}
  defp quoted(d, <<c::utf8, rest::binary>>, str), do: quoted(d, rest, <<str::binary, c::utf8>>)

  # read a raw unquoted string up to the next delimiter, newline or EOF
  # do not consume the following delimiter, EOL, EOF
  # because the delimiter is needed to recognize trailing empty value
  @spec raw(char(), String.t(), String.t()) :: {String.t(), String.t()}
  defp raw(d, <<d::utf8, _::binary>> = rest, val), do: {val, rest}
  defp raw(_, <<?\n, _::binary>> = rest, val), do: {val, rest}
  defp raw(d, <<c::utf8, rest::binary>>, val), do: raw(d, rest, <<val::binary, c::utf8>>)
  defp raw(_, <<>>, val), do: {val, <<>>}
end
