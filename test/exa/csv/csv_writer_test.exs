defmodule Exa.Csv.CsvWriterTest do
  use ExUnit.Case

  use Exa.Csv.Constants

  import Exa.Csv.CsvWriter

  @out_dir ["test", "output", "csv"]

  defp file(name), do: Exa.File.join(@out_dir, name, @filetype_csv)

  @simple_csv ~s|a,1,3.14,foo,true
abcd !@#$%^&,9999,,bar,false
"abc ""def"" ghi",5,0.0,baz,true
"abc \n ghi",-5,0.7857,raz,true
|

  @null_csv ~s|a,1,3.14,foo,true
abcd !@#$%^&,9999,null,bar,false
"abc ""def"" ghi",5,0.0,baz,true
"abc \n ghi",-5,0.7857,raz,true
|

  @simple_map ~s|atom,float,bool,int,text
foo,3.14,true,1,a
bar,null,false,9999,abcd !@#$%^&
baz,0.0,true,5,\"abc \"\"def\"\" ghi\"
raz,0.7857,true,-5,\"abc \n ghi\"
|

  @datetime_csv ~s|2023-12-02T13:26:08.003000Z,2023-12-02,13:26:08.003000
2023-12-15T09:42:13.875000Z,2023-12-15,09:42:13.875000
|

  # CSV output ---------

  test "simple list" do
    rows = [
      ["a", 1, 3.14, :foo, true],
      ["abcd !@#$%^&", 9999, nil, :bar, false],
      [~s|abc "def" ghi|, 5, 0.0, :baz, true],
      [~s|abc \n ghi|, -5, 0.7857, :raz, true]
    ]

    csv = to_file(rows, file("simple-list"))
    assert @simple_csv == to_string(csv)

    assert {:error, _} = to_file(rows, [header: true], "simple-list")
  end

  test "simple keyword" do
    rows = [
      [text: "a", int: 1, float: 3.14, atom: :foo, bool: true],
      [text: "abcd !@#$%^&", int: 9999, float: nil, atom: :bar, bool: false],
      [text: ~s|abc "def" ghi|, int: 5, float: 0.0, atom: :baz, bool: true],
      [text: ~s|abc \n ghi|, int: -5, float: 0.7857321, atom: :raz, bool: true]
    ]

    csv = to_file(rows, file("simple-keyw"))
    assert @simple_csv == to_string(csv)
  end

  test "simple map" do
    rows = [
      %{:text => "a", :int => 1, :float => 3.14, :atom => :foo, :bool => true},
      %{:text => "abcd !@#$%^&", :int => 9999, :float => nil, :atom => :bar, :bool => false},
      %{:text => ~s|abc "def" ghi|, :int => 5, :float => 0.0, :atom => :baz, :bool => true},
      %{:text => ~s|abc \n ghi|, :int => -5, :float => 0.7857321, :atom => :raz, :bool => true}
    ]

    csv = to_file(rows, [null: :null, header: true], file("simple-maps"))
    assert @simple_map == to_string(csv)
  end

  test "index map" do
    rows = [
      %{0 => "a", 1 => 1, 2 => 3.14, 3 => :foo, 4 => true},
      %{0 => "abcd !@#$%^&", 1 => 9999, 2 => nil, 3 => :bar, 4 => false},
      %{0 => ~s|abc "def" ghi|, 1 => 5, 2 => 0.0, 3 => :baz, 4 => true},
      %{0 => ~s|abc \n ghi|, 1 => -5, 2 => 0.7857321, 3 => :raz, 4 => true}
    ]

    # no header and result is same as original simple csv
    csv = to_file(rows, [null: :null], file("simple-maps"))
    assert @null_csv == to_string(csv)
  end

  test "datetimes list" do
    {:ok, d1} = Date.new(2023, 12, 2)
    {:ok, t1} = Time.new(13, 26, 8, 3000)
    {:ok, d2} = Date.new(2023, 12, 15)
    {:ok, t2} = Time.new(9, 42, 13, 875_000)

    rows = [
      [DateTime.new!(d1, t1), d1, t1],
      [DateTime.new!(d2, t2), d2, t2]
    ]

    csv = to_file(rows, file("simple-list"))
    assert @datetime_csv == to_string(csv)
  end
end
