# cob2csv

[![CI](https://github.com/HorseyofCoursey/cob2csv/actions/workflows/ci.yml/badge.svg)](https://github.com/HorseyofCoursey/cob2csv/actions/workflows/ci.yml)

A pair of command-line tools written in pure COBOL that convert fixed-width flat files to CSV and back. Built with [GnuCOBOL](https://gnucobol.sourceforge.io/) — no mainframe required.

```
000001Anderson       Jon            000150099NY
000002Brown          Billie         000000000TX
                        ↕  cob2csv / csv2cob
CUST-ID,FIRST-NAME,LAST-NAME,BALANCE,STATE
1,Anderson,Jon,1500.99,NY
2,Brown,Billie,0.00,TX
```

## Tools

| Tool | Direction |
|---|---|
| `cob2csv` | Fixed-width COBOL flat file → CSV |
| `csv2cob` | CSV → Fixed-width COBOL flat file |

## Requirements

```bash
sudo apt install gnucobol   # Ubuntu/Debian
brew install gnucobol       # macOS
```

## Build

```bash
make
```

Binaries are placed in `bin/`.

## Usage

### cob2csv — flat file to CSV

```bash
cob2csv -s schema.cfg -i input.dat -o output.csv

# or pipe to stdout
cob2csv -s schema.cfg -i input.dat
```

### csv2cob — CSV to flat file

```bash
csv2cob -s schema.cfg -i input.csv -o output.dat
```

### Flags

| Flag | Long form | Description |
|---|---|---|
| `-s` | `--schema` | Schema config file (required) |
| `-i` | `--input` | Input file (required) |
| `-o` | `--output` | Output file (default: stdout for cob2csv) |

## Schema format

Define your record layout in a plain `.cfg` file. Each `FIELD` line describes one column:

```
# customer.cfg
FIELD NAME=CUST-ID     START=1   LENGTH=6   TYPE=NUM
FIELD NAME=FIRST-NAME  START=7   LENGTH=15  TYPE=ALPHA
FIELD NAME=LAST-NAME   START=22  LENGTH=15  TYPE=ALPHA
FIELD NAME=BALANCE     START=37  LENGTH=9   TYPE=DECIMAL DECIMALS=2
FIELD NAME=STATE       START=46  LENGTH=2   TYPE=ALPHA
```

| Key | Description |
|---|---|
| `NAME` | Column name (used as CSV header) |
| `START` | 1-based start position in the fixed-width record |
| `LENGTH` | Field length in characters |
| `TYPE` | `ALPHA`, `NUM`, or `DECIMAL` |
| `DECIMALS` | Number of implied decimal places (DECIMAL type only) |

### Types

- **`ALPHA`** — Text. Left-justified in output, space-padded in flat file. Quoted in CSV if the value contains a comma or quote.
- **`NUM`** — Integer. Right-justified, zero-padded in flat file. Leading zeros stripped in CSV.
- **`DECIMAL`** — Implied decimal (no decimal point stored). `LENGTH=9 DECIMALS=2` means 7 integer digits + 2 fractional digits. Decimal point inserted on CSV output.

## Running tests

```bash
make test
```

Tests use `diff` to compare actual output against expected fixtures — no test framework needed.

## Examples

See the `tests/` directory for a working customer schema and data file. The `examples/` directory contains additional schema samples.

## Why COBOL?

Fixed-width flat files are COBOL's native habitat. Banks, insurers, and government agencies still produce them by the millions. This tool is written in the same language that generated those files — which means it handles the data model correctly by design, not by approximation.

## License
cob2csv is free for personal and non-commercial use under the
[PolyForm Noncommercial License 1.0.0](LICENSE).

If you or your organization want to use cob2csv in a commercial context,
a commercial license is required. Please refer to COMMERCIAL.md 
