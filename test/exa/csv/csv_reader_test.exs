defmodule Exa.Csv.CsvReaderTest do
  use ExUnit.Case

  use Exa.Csv.Constants

  import Exa.Csv.CsvReader

  @bench_dir Path.join(["test", "bench"])

  @in_dir Path.join(["test", "input", "csv"])

  @csv_dir Path.join(["test", "input", "csv", "datablist"])

  @sizes ["100", "1000", "10000", "100000"]
  # "500000", "1000000", "2000000"]

  defp in_file(name), do: Exa.File.join(@in_dir, name, @filetype_csv)

  defp csv_file(name, size) do
    Exa.File.join(@csv_dir, name <> "-" <> size, [@filetype_csv, @filetype_gz])
  end

  @simple_rows [
    ["a", 1, 3.14, "foo", true],
    ["abcd !@#$%^&", 99, nil, "bar", false],
    [~s|abc "def" g,h,i|, 5, 0.0, nil, nil],
    [~s|abc \n ghi|, -5, nil, nil, true]
  ]

  @datetime_rows [
    [{~U[2023-12-02 13:26:08.003000Z], 0}, ~D[2023-12-02], ~T[13:26:08.003000]],
    [{~U[2023-12-15 09:42:13.875000Z], 0}, ~D[2023-12-15], ~T[09:42:13.875000]]
  ]

  @header_rows [
    ["abcd", 99, nil, "bar", false],
    [~s|abc ghi|, 5, 0.0, nil, true]
  ]

  # CSV output ---------

  test "simple" do
    {:csv, csv, :list} = "simple" |> in_file() |> from_file()
    assert @simple_rows = csv
  end

  test "datetimes" do
    {:csv, csv, :list} = "datetimes"  |> in_file() |> from_file()
    assert @datetime_rows == csv
  end

  test "header" do
    {:csv, csv, [:string, :int, :float, :atom__string, :bool]} =
      "header"  |> in_file() |> from_file(header: true)

    assert @header_rows = csv
  end

  # large input files with/without benchmarking ----------

  test "datablist" do
    # functional test just one size
    bench = benchmarks(["100"])
    results = bench |> Map.values() |> Enum.map(fn fun -> fun.() end)
    IO.inspect(results)
  end

  @tag benchmark: true
  @tag timeout: 300_000
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
        {"customers-" <> sz, fn -> 
          "customers" |> csv_file(sz) |> from_file(header: true) 
        end}
      end

    results =
      for sz <- sizes, into: results do
        {"people-" <> sz, fn -> 
          "people" |> csv_file(sz) |> from_file(header: true) 
      end}
      end

      for sz <- sizes, into: results do
        {"organizations-" <> sz, fn -> 
          "organizations" |> csv_file(sz) |> from_file(header: true) 
        end}
      end
  end
end
