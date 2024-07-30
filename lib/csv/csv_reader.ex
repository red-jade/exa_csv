defmodule Exa.Csv.CsvReader do
  @moduledoc "Utilities to parse CSV format."

  require Logger

  use Exa.Constants

  import Exa.Types
  alias Exa.Types, as: E

  alias Exa.Parse, as: P

  import Exa.Csv.Types
  alias Exa.Csv.Types, as: C

  alias Exa.Option
  alias Exa.Parse

  # TODO - remove after next revision of core Exa.Constants
  @nulls ["", "null", "nil", "nan", "inf"]

  # -----------
  # local types
  # -----------

  # map of column keys to specific parsers
  @typep parsers() :: %{C.key() => P.parfun(any())}

  # column key data type for parser map and output records
  @typep key_type() :: :atom | :int | :string

  # temporary key type, includes `:index` for update from first row
  @typep key_vals() :: nil | :index | [C.key()]

  # key data for parser map: type and key values
  @typep par_data() :: {key_type(), key_vals()}

  @doc """
  Read a CSV file.

  The file is assumed to be encoded in UTF-8 
  (UTF16/UTF32 LE/BE not supported).
  If the file has a UTF-8 BOM, it will be ignored.

  See `decode/2` for descritption of the `:options`.
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

  The result is a list of column keys 
  and a list of row data records.

  The records can be lists, maps, keyword lists,
  or custom structs.

  ## Column Keys

  The column keys can be atoms, strings or 
  0-based integers.
  Column keys can be passed as an option,
  read from a header row in the input,
  or indexed from the sequence of columns.

  If the CSV file has a header line, 
  and the `:header` option is `true`,
  then column names are read from the first row of the data.

  If the CSV file does not have a header line, 
  the column names can be explicitly provided 
  in the `:columns` option (list of strings or atoms).
  If there are column names provided as an option
  and a header row, then the option value takes precedence.

  If column names are not available in either header or options, 
  then the column keys will be 0-based integers,
  generated from the number of columns in the first row.

  Column keys are used to: 
  - look-up column-specific parser functions
  - used as keys to build row records (keyword, map, struct)
  - returned as the second element in the result

  Column name strings are converted to atom keys by:
  trimming all whitespace, reducing internal runs of whitespace 
  to a single underscore `'_'`, removing all non-alphanumeric characters,
  and downcasing alphabetic characters.

  ## Parsing Rows

  There are three stages to process each row:
  - cell content is unescaped and unquoted to be a string value
  - each cell string is optionally parsed into another datatype
  - each row is assembled into a record 
    (list, keyword, map or custom struct)

  ### Cell Parsers

  There are three levels to parsing each cell string value:
  - individual column-specific parser (`:parsers` option)
  - custom generic default parser (`:pardef` option)
  - fallback default parser that tries to guess the data type
    and takes a list of nullable strings that yield `nil` 
    (`:nulls` option) 

  Each parser function must be:<br>
     `(nil | String.t() -> nil | String.t() | xyz())`

  A `nil` argument should always be passed through as a `nil` result.
  The function should not raise errors. 
  Any errors will be caught and replaced with the value `{:error, input}`.

  Individual column parsers can be specified in the `:parsers` option.
  The parser map is keyed by column key (atom, string or 0-based integer).
  One or more parsers can be provided, not every column must have a parser.
  The parser value must be a function `(String.t() -> any())`.

  The type of keys actually used in the parsers map option will be 
  used to convert column names and headers to the correct key format.

  If no custom parser is specified for a particular column,
  then the default parser is used (potentially for all columns).
  A default parser can be provided using the `:pardef` option.

  If no default parser is provided as an option,
  then the fallback default parser is `Exa.Parse.guess/4`.
  It will convert `nil` values using the optional `:nulls` option
  or use the default set `#{@nulls}`. 
  Then it will identify booleans, 
  integers, floats, DateTimes, Dates and Times.
  For temporal types, the ISO 8601 formats are assumed.

  See `Exa.Parse` for:
  - collection of specific data parsers 
  - `Exa.Parse.compose/1` function to cascade simple parsers
  - `Exa.Parse.guess/4` that matches many simple types

  ## Building Records

  The `:record` options determines how rows are returned.
  The option value should be either:
  - `List` module name, forces simple list, without any keys
  - module name that has a `new/1` function 
    taking a list of key-value pairs:
    - `Keyword` forces use of atom column keys
    - `Map` can use any key type given in the `:outkey` option
       as either `:int | :atom | :string`
    - custom module with the required `new/1` function
  - factory function taking a Keyword list argument
    that forces the use of atom column keys

  A custom module can create application-specific objects
  based on the set of keys in the list.

  The `Exa.Factory` can generate a factory function from a list of structs.
  It matches keys to pick the correct struct from the provided repertoire.

  If the `:record` option is `List`, the `:outkey` option
  still determines the format of the column keys returned in the 
  second element of the result.

  ## Options

  - `:delim` the character delimiter between fields. 
    Default: comma `?,`, other common values include `?|` or `?\\t`.

  - `:header` boolean flag to control reading of an 
    optional column header line. Column headers must be unique.
    Default: `false`.

  - `:columns` the list of column names (strings or atoms).
    Column names are used: to look-up parsers to read data;
    as keys to build data objects for each row;
    and returned as the second element of the result. 
    Column names must be unique.

  - `:parsers` a map of parser functions for specific columns. 
    The key is a column name converted to an atom,
    or a 0-based index of the column number.

  - `:pardef` a default parser that will be invoked 
    if there is no specific parser for the column.

  - `:nulls` a list of case-insensitive string values 
    that will be converted to `nil` by the 
    default default parser (`Exa.Parse.guess/4`).
    The default values are: `#{@nulls}`.
    The fallback default parser will only be invoked when there 
    is no custom column-specific parser, 
    and no generic default parser specified in the options.
  """
  @spec decode(String.t(), E.options()) :: C.read_csv()
  def decode(csv, opts \\ []) when is_string(csv) do
    # the delimiter for the fields in a row
    delim = Option.get_char(opts, :delim, ?,)

    # TODO - change header to be a nonneg int count of rows to ignore

    # if head? is true, then cols are read from first non-empty row
    # else cols will be the column names option
    # otherwise it will be empty list []

    head? = Option.get_bool(opts, :header, false)
    cols = Option.get_list_nonempty_name(opts, :columns, [])
    {cols, csv} = headers(head?, delim, csv, cols)

    # options for parsers, there are three levels:
    # - column-specific parsers
    # - default parser
    # - null values for the fallback default parser

    nulls = Option.get_list_string(opts, :nulls, [])
    nulls = if nulls == [], do: @nulls, else: Enum.map(nulls, &String.downcase/1)
    pardef = Option.get_fun(opts, :pardef, Parse.guess(nulls))
    parsers = Option.get_map(opts, :parsers, %{})

    # get the sequence of parser keys
    # use the parser map keys to determine type
    # match with number, value(?) of actual col/hdr values

    # if parsers exist and are indexed with ints
    # then use number of cols to determine int range sequence
    # if no cols, set pkeys as :index so int keys taken from first row

    # if no parsers, pkeys is nil
    # and ptype defaults to :string as default for outkey

    # ptype - :atom | :string | :int
    # pkeys - nil | :index | [key]

    {ptype, pkeys} = pdata = par_keys(parsers, cols, :string)

    # TODO - upgrade to Option.get_enum on next revision of Exa core

    # record constructor is either:
    # - module List forces output to simple list, no 'new' func, no keys
    # - module with appropriate 'new/1' function:
    #   - Keyword overrides outkey type to be atom (must be cols)
    #   - Map, allows any outkey type
    #   - custom module, allows any outkey type
    # - specific struct factory function
    #   overrides outkey type to be atom (must be columns)

    rec = Keyword.get(opts, :record, List)

    if is_module(rec) and rec != List and not function_exported?(rec, :new, 1) do
      error("Record factory module #{rec} must export 'new/1' function")
    end

    # output key type for Map record - :int | :atom | :string
    # atom or string keys must have cols source 
    # defaults to the parser key type

    outkey = Option.get_atom(opts, :outkey, ptype)

    if rec == Map and outkey not in [:int, :atom, :string] do
      error("Record Map must have valid outkey type: :int, :atom, :string")
    end

    # okeys  - :list | :index | [key]
    # facfun - nil | fun/1

    {okeys, facfun} =
      case rec do
        List -> {:list, nil}
        Keyword -> {out_keys(:atom, pdata), &Keyword.new/1}
        Map -> {out_keys(outkey, pdata), &rec.new/1}
        mod when is_module(mod) -> {out_keys(:atom, pdata), &rec.new/1}
        fun when is_function(fun, 1) -> {out_keys(:atom, pdata), rec}
        _ -> error("Illegal options: record '#{rec}' outkey '#{outkey}'")
      end

    omap = %{
      :delim => delim,
      :parsers => parsers,
      :pkeys => pkeys,
      :pardef => pardef,
      :okeys => okeys,
      :facfun => facfun
    }

    # pkeys is now -  nil  | [key]
    # okeys is now - :list | [key]

    {okeys, recs} = rows(csv, omap, [])

    # allow output of header keys or strings 
    # even if records will be lists

    okeys =
      cond do
        okeys == :list and is_list(pkeys) -> out_keys(outkey, pdata)
        true -> okeys
      end

    {:csv, okeys, recs}
  end

  # -----------------
  # private functions 
  # -----------------

  # read a list of headers from the first row of the csv
  @spec headers(bool(), char(), String.t(), C.columns()) :: {C.columns(), String.t()}

  defp headers(true, delim, csv, cols) do
    {hdrs, rest} =
      case vals(delim, csv, []) do
        # ignore empty row
        {[], rest} -> headers(true, delim, rest, cols)
        result -> result
      end

    # TODO - compare cols options with actual header names?
    if cols != [] and length(cols) != length(hdrs) do
      error("Column header mismatch, expected #{cols}, found #{hdrs}")
    end

    # specified columns override headers 
    # as keys for parser look-up and default for object output
    keys = if cols != [], do: cols, else: hdrs

    {keys, rest}
  end

  defp headers(false, _delim, csv, cols), do: {cols, csv}

  # build the keys used to lookup parsers:
  # - nil means no keys needed (no parsers to lookup)
  # - index means use first row to build int range keys
  # - list of atom, string or int keys, with type
  @spec par_keys(parsers(), C.columns(), key_type()) :: par_data()

  defp par_keys(parsers, _, deftype) when parsers == %{}, do: {deftype, nil}

  defp par_keys(parsers, cols, _) when is_map(parsers) do
    mkeys = Map.keys(parsers)

    ptype =
      cond do
        Enum.all?(mkeys, &is_atom/1) -> :atom
        Enum.all?(mkeys, &is_nonneg_int/1) -> :int
        Enum.all?(mkeys, &is_string/1) -> :string
        true -> error("Parser keys must be ints, atoms or strings, found #{mkeys}")
      end

    pkeys =
      case ptype do
        :atom when cols != [] -> Enum.map(cols, &to_downatom/1)
        :string when cols != [] -> Enum.map(cols, &to_string/1)
        :int when cols != [] -> Range.to_list(0..(length(cols) - 1))
        :int -> :index
        _atom_or_string -> error("Keys must be ints if no columns, found #{mkeys}")
      end

    if not MapSet.subset?(MapSet.new(mkeys), MapSet.new(pkeys)) do
      error(
        "Parser keys '#{inspect(mkeys)}', " <>
          "not in index range or column names '#{inspect(pkeys)}'"
      )
    end

    {ptype, pkeys}
  end

  # convert parser key data to required output key type
  @spec out_keys(key_type(), par_data()) :: key_vals()

  defp out_keys(:int, {_, nil}), do: :index
  defp out_keys(:int, {:int, :index}), do: :index
  defp out_keys(:int, {:int, keys}), do: keys
  defp out_keys(:int, {_, keys}), do: Range.to_list(0..(length(keys) - 1))
  defp out_keys(:atom, {:atom, keys}), do: keys
  defp out_keys(:atom, {:string, keys}), do: Enum.map(keys, &to_downatom/1)
  defp out_keys(:string, {:string, keys}), do: keys
  defp out_keys(:string, {:atom, keys}), do: Enum.map(keys, &to_string/1)

  defp out_keys(otype, {ptype, _}),
    do: error("Cannot convert keys from #{ptype} to #{otype}")

  # ------
  # parser 
  # ------

  # read a list of records from the rows of the csv up to EOF
  @spec rows(String.t(), map(), C.records(), bool()) :: {:list | C.keys(), C.records()}
  defp rows(csv, omap, rows, first? \\ true)

  defp rows(<<>>, omap, rows, _), do: {omap.okeys, Enum.reverse(rows)}

  defp rows(csv, omap, rows, first?) do
    case vals(omap.delim, csv, []) do
      # ignore empty rows
      {[], rest} ->
        rows(rest, omap, rows, first?)

      {vals, rest} ->
        # use first row to set index integer keys 
        omap = if first?, do: update_keys(length(vals), omap), else: omap
        rows(rest, omap, [row(vals, omap) | rows], false)
    end
  end

  # use first row to set the number of columns for indexed keys
  @spec update_keys(E.count1(), map()) :: map()
  defp update_keys(nval, omap) do
    ikeys = Range.to_list(0..(nval - 1))
    omap = if omap.pkeys == :index, do: %{omap | :pkeys => ikeys}, else: omap
    omap = if omap.okeys == :index, do: %{omap | :okeys => ikeys}, else: omap
    omap
  end

  # build a record from values in the row
  @spec row([C.value()], map()) :: C.record()
  defp row(vals, omap) when vals != [] do
    if omap.okeys != :list and length(omap.okeys) != length(vals) do
      Logger.info("Keys #{inspect(omap.okeys, limit: 100)}")
      Logger.info("Vals #{inspect(vals, limit: 100)}")

      error(
        "Mismatch number of columns, " <>
          "expect #{length(omap.okeys)}, found #{length(vals)}"
      )
    end

    vals
    |> parse(omap.pkeys, omap.pardef, omap.parsers, [])
    |> build(omap.okeys, omap.facfun)
  end

  # parse the values for nil and custom types
  @spec parse(C.values(), nil | C.keys(), P.parfun(any()), parsers(), [any()]) :: [any()]

  defp parse([], _, _, _, pvals), do: Enum.reverse(pvals)

  defp parse([v | vals], nil, pardef, parsers, pvals) do
    # no specific parser, just default
    pv = do_parse(v, nil, pardef)
    parse(vals, nil, pardef, parsers, [pv | pvals])
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

  # -----
  # lexer
  # -----

  # read a list of values in one row of the csv up to EOL or EOF
  @spec vals(char(), String.t(), [C.value()]) :: {[C.value()], binary()}

  # quoted subsequent columns have a leading delimiter
  defp vals(d, <<d::utf8, ?", rest::binary>>, vals) do
    {v, rest} = quoted(d, rest)
    vals(d, rest, [v | vals])
  end

  # quoted first column does not have a leading delimiter
  defp vals(d, <<?", rest::binary>>, vals) do
    {v, rest} = quoted(d, rest)
    vals(d, rest, [v | vals])
  end

  # raw subsequent columns have a leading delimiter
  defp vals(d, <<d::utf8, rest::binary>>, vals) do
    # consume spaces before a quote
    {v, rest} =
      case lead_spaces(rest) do
        :raw -> raw(d, rest)
        {:quoted, rest} -> quoted(d, rest)
      end

    vals(d, rest, [v | vals])
  end

  # EOL
  defp vals(_, <<?\n, rest::binary>>, vals), do: {Enum.reverse(vals), rest}

  # EOF
  defp vals(_, <<>>, vals), do: {Enum.reverse(vals), <<>>}

  # raw first column does not have a leading delimiter
  defp vals(d, <<rest::binary>>, vals) do
    # consume spaces before a quote
    {v, rest} =
      case lead_spaces(rest) do
        :raw -> raw(d, rest)
        {:quoted, rest} -> quoted(d, rest)
      end

    vals(d, rest, [v | vals])
  end

  # build objects from keys and row values 
  @spec build(C.values(), :list | C.keys(), nil | Exa.Factory.factory_fun()) :: any()
  defp build(vals, :list, nil), do: vals
  defp build(vals, okeys, facfun), do: okeys |> Enum.zip(vals) |> facfun.()

  # read a raw unquoted string up to the next delimiter, newline or EOF
  # do not consume the following delimiter, EOL, EOF
  # because the delimiter is needed to recognize trailing empty value
  @spec raw(char(), String.t(), String.t()) :: {String.t(), String.t()}
  defp raw(delim, str, out \\ <<>>)
  defp raw(d, <<d::utf8, _::binary>> = rest, val), do: {val, rest}
  defp raw(_, <<?\n, _::binary>> = rest, val), do: {val, rest}
  defp raw(d, <<c::utf8, rest::binary>>, val), do: raw(d, rest, <<val::binary, c::utf8>>)
  defp raw(_, <<>>, val), do: {val, <<>>}

  # TODO - handle error: read to end of line when quoted string ends with ""

  # read a quoted string up to the close quotes
  # consume trailing spaces
  # consume any following delimiter, but not EOL or EOF
  @spec quoted(char(), String.t(), String.t()) :: {String.t(), String.t()}
  defp quoted(delim, str, out \\ <<>>)
  defp quoted(d, <<?", ?", rest::binary>>, s), do: quoted(d, rest, <<s::binary, ?">>)
  defp quoted(d, <<?", d::utf8, rest::binary>>, s), do: {s, rest}
  defp quoted(d, <<?", ?\s, rest::binary>>, s), do: {s, trail_spaces(d, rest)}
  defp quoted(_, <<?", rest::binary>>, s), do: {s, rest}
  defp quoted(_, <<>>, s), do: error("Missing close quote '#{s}'")
  defp quoted(d, <<c::utf8, rest::binary>>, s), do: quoted(d, rest, <<s::binary, c::utf8>>)

  # consume leading space until double quote
  @spec lead_spaces(String.t()) :: :raw | {:quoted, String.t()}
  defp lead_spaces(<<?\s, rest::binary>>), do: lead_spaces(rest)
  defp lead_spaces(<<?", rest::binary>>), do: {:quoted, rest}
  defp lead_spaces(_), do: :raw

  # consume spaces following close quote
  # consume any following delimiter, but not EOL or EOF
  @spec trail_spaces(char(), String.t()) :: String.t()
  defp trail_spaces(d, <<?\s, rest::binary>>), do: trail_spaces(d, rest)
  defp trail_spaces(d, <<d::utf8, rest::binary>>), do: rest
  defp trail_spaces(_, rest), do: rest

  # convert a name (atom or String) to an atom
  @spec to_downatom(E.name()) :: atom()

  defp to_downatom(a) when is_atom(a), do: a

  defp to_downatom(s) when is_string(s) do
    s |> Exa.String.sanitize!() |> String.downcase() |> String.to_atom()
  end

  # move something like this into Exa core?
  defp error(msg) do
    msg = String.replace(msg, "\n", " ")
    Logger.error(msg)
    raise ArgumentError, message: msg
  end
end
