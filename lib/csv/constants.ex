defmodule Exa.Csv.Constants do
  @moduledoc "Constants for CSV format."

  defmacro __using__(_) do
    quote do
      @filetype_csv :csv
      @filetype_gz :gz
    end
  end
end
