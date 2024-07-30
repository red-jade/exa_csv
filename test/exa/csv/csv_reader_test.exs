defmodule Exa.Csv.CsvReaderTest do
  use ExUnit.Case

  require Logger

  use Exa.Csv.Constants

  alias Exa.Parse

  import Exa.Csv.CsvReader

  @bench_dir Path.join(["test", "bench"])

  @in_dir Path.join(["test", "input", "csv"])

  @csv_dir Path.join(["test", "input", "csv", "datablist"])

  @sizes ["100", "1000", "10000", "100000"]
  # "500000", "1000000", "2000000"]

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_csv)

  defp csv_file(name, size) do
    Exa.File.join(@csv_dir, name <> "-" <> size, @filetype_csv)
    # , @filetype_gz])
  end

  @simple_rows [
    %{0 => "abcd", 1 => 1, 2 => 3.14, 3 => "foo", 4 => true},
    %{0 => "!@#$%^&", 1 => 99, 2 => nil, 3 => "bar", 4 => false},
    %{0 => ~s|abc "def" g,h,i|, 1 => 5, 2 => 0.0, 3 => nil, 4 => nil},
    %{0 => ~s|abc \n ghi|, 1 => -5, 2 => nil, 3 => nil, 4 => true}
  ]

  @datetime_rows [
    [{~U[2023-12-02 13:26:08.003000Z], 0}, ~D[2023-12-02], ~T[13:26:08.003000]],
    [{~U[2023-12-15 09:42:13.875000Z], 0}, ~D[2023-12-15], ~T[09:42:13.875000]]
  ]

  # @header_rows [
  #   ["abcd", 99, nil, :bar, false],
  #   ["abc ghi", 5, 0.0, nil, true]
  # ]

  @header_amaps [
    %{:string => "abcd", :int => 99, :float => nil, :atom => :bar, :bool => false},
    %{:string => "abc ghi", :int => 5, :float => 0.0, :atom => nil, :bool => true}
  ]

  # CSV output ---------

  test "simple" do
    ixs = Range.to_list(0..4)
    {:csv, ^ixs, csv} = "simple" |> in_file() |> from_file(record: Map, outkey: :int)
    assert @simple_rows = csv
  end

  test "quoted error" do
    assert {:error, %ArgumentError{message: "Missing close quote 'a\" ,b,c"<>_}} =
             "quoterr1" |> in_file() |> from_file(record: Map, outkey: :int)

    assert {:error, %ArgumentError{message: "Missing close quote 'q,r"<> _}} =
             "quoterr2" |> in_file() |> from_file(record: Map, outkey: :int)

    assert {:error, %ArgumentError{message: "Missing close quote 'r\""<>_}} =
             "quoterr3" |> in_file() |> from_file(record: Map, outkey: :int)

    assert {:error, %ArgumentError{message: "Missing close quote 'z\"'"}} =
             "quoterr4" |> in_file() |> from_file(record: Map, outkey: :int)
  end

  test "datetimes" do
    {:csv, :list, csv} = "datetimes" |> in_file() |> from_file()
    assert @datetime_rows == csv
  end

  test "header" do
    keys = [:string, :int, :float, :atom, :bool]

    pars = %{
      # nullable string
      :string => Parse.null(),
      :int => Parse.int(),
      :float => Parse.compose([Parse.null(), Parse.float()]),
      :atom => Parse.compose([Parse.null(), Parse.atom(["foo", "bar"])]),
      :bool => Parse.bool()
    }

    {:csv, ^keys, csv} =
      "header"
      |> in_file()
      |> from_file(header: true, parsers: pars, record: Map, outkey: :atom)

    assert @header_amaps = csv
  end

  # same input files with/without benchmarking ----------

  defp cpars() do
    p_string = Parse.string()

    %{
      :index => Parse.int(),
      :customer_id => Parse.string(&(String.length(&1) > 0)),
      :first_name => p_string,
      :last_name => p_string,
      :company => p_string,
      :city => p_string,
      # TODO - ISO country parser? exa_i18n
      :country => p_string,
      :phone_1 => p_string,
      :phone_2 => p_string,
      # Parse.email(),
      :email => p_string,
      :subscription_date => Parse.date(),
      :website => Parse.uri()
    }
  end

  defp ppars() do
    p_string = Parse.string()

    %{
      :index => Parse.int(),
      # should use the new Parse.string() here - after roll core version
      :user_id => Parse.string(&(String.length(&1) > 0)),
      :first_name => p_string,
      :last_name => p_string,
      :sex => Parse.atom(["male", "female"]),
      # Parse.email(),
      :email => p_string,
      :phone => p_string,
      :date_of_birth => Parse.date(),
      :job_title => p_string
    }
  end

  defp opars() do
    p_string = Parse.string()

    %{
      :index => Parse.int(),
      :organization_id => Parse.string(&(String.length(&1) > 0)),
      :name => p_string,
      :website => Parse.uri(),
      :country => p_string,
      :description => p_string,
      :founded => Parse.int(),
      :industry => p_string,
      :number_of_employees => Parse.int()
    }
  end

  test "datablist" do
    # functional test just one size
    sz = "100"

    {:csv, ccols, cust} =
      "customers"
      |> csv_file(sz)
      |> from_file(header: true, parsers: cpars(), record: List, outkey: :atom)

    assert ccols == [
             :index,
             :customer_id,
             :first_name,
             :last_name,
             :company,
             :city,
             :country,
             :phone_1,
             :phone_2,
             :email,
             :subscription_date,
             :website
           ]

    assert [
             1,
             "DD37Cf93aecA6Dc",
             "Sheryl",
             "Baxter",
             "Rasmussen Group",
             "East Leonard",
             "Chile",
             "229.077.5154",
             "397.884.0519x718",
             "zunigavanessa@smith.info",
             ~D[2020-08-24],
             %URI{
               scheme: "http",
               authority: "www.stephenson.com",
               userinfo: nil,
               host: "www.stephenson.com",
               port: 80,
               path: "/",
               query: nil,
               fragment: nil
             }
           ] == hd(cust)

    {:csv, pcols, peop} = "people" |> csv_file(sz) |> from_file(header: true, parsers: ppars())

    assert pcols == [
             :index,
             :user_id,
             :first_name,
             :last_name,
             :sex,
             :email,
             :phone,
             :date_of_birth,
             :job_title
           ]

    assert [
             1,
             "88F7B33d2bcf9f5",
             "Shelby",
             "Terrell",
             :male,
             "elijah57@example.net",
             "001-084-906-7849x73518",
             ~D[1945-10-26],
             "Games developer"
           ] = hd(peop)

    {:csv, ocols, orgs} =
      "organizations" |> csv_file(sz) |> from_file(header: true, parsers: opars())

    assert ocols == [
             :index,
             :organization_id,
             :name,
             :website,
             :country,
             :description,
             :founded,
             :industry,
             :number_of_employees
           ]

    assert [
             1,
             "FAB0d41d5b5d22c",
             "Ferrell LLC",
             %URI{
               scheme: "https",
               authority: "price.net",
               userinfo: nil,
               host: "price.net",
               port: 443,
               path: "/",
               query: nil,
               fragment: nil
             },
             "Papua New Guinea",
             "Horizontal empowering knowledgebase",
             1990,
             "Plastics",
             3498
           ] = hd(orgs)
  end

  # benchmark

  @tag benchmark: true
  @tag timeout: 400_000
  test "datablist benchmarks" do
    # performance test all sizes
    Benchee.run(
      benchmarks(@sizes),
      time: 20,
      save: [path: Path.join(@bench_dir, "csv_reader.benchee")],
      load: Path.join(@bench_dir, "csv_reader.latest.benchee")
    )
  end

  defp benchmarks(sizes) do
    results =
      for sz <- sizes, into: %{} do
        {"customers-" <> sz,
         fn ->
           "customers" |> csv_file(sz) |> from_file(header: true)
           # , parsers: cpars())
         end}
      end

    results =
      for sz <- sizes, into: results do
        {"people-" <> sz,
         fn ->
           "people" |> csv_file(sz) |> from_file(header: true)
           # , parsers: ppars())
         end}
      end

    for sz <- sizes, into: results do
      {"organizations-" <> sz,
       fn ->
         "organizations" |> csv_file(sz) |> from_file(header: true)
         # , parsers: opars())
       end}
    end
  end
end
