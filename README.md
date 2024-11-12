## EXA Csv

𝔼𝕏tr𝔸 𝔼li𝕏ir 𝔸dditions (𝔼𝕏𝔸)

EXA project index: [exa](https://github.com/red-jade/exa)

Utilities for reading and writing CSV files.

Module path: `Exa.Csv`

### Features

- Read CSV files, with configurable parsers and data conversion
- Write CSV files 
- See `exa_gis` for lat/lon location parsers

## Building

**To bootstrap an `exa_xxx` library build,**<br>
**you must update dependencies twice.**

One way to start a build is:
```
  $ mix deps.update exa
  $ mix deps.get
```

## Benchmarks

Exa uses _Benchee_ for performancee testing.

Test results are stored under `test/bench/*.benchee`.
The current _latest_ baseline and previous results are checked-in.

Run the benchmarks and compare with latest result:

`$ mix test --only benchmark:true`

To run specific benchmark test, for example:

`$ mix test --only benchmark:true test/exa/image/image_test.exs`

## Test Data

Test CSV data downloaded from:<br>
https://www.datablist.com/learn/csv/download-sample-csv-files

The scripts to generate those files are available:<br>
https://github.com/datablist/sample-csv-files

## EXA License

EXA source code is released under the MIT license.

EXA code and documentation are:<br>
Copyright (c) 2024 Mike French
