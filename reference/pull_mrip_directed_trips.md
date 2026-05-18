# Extract MRIP Directed Trip Estimates for Multiple Species

Downloads and processes MRIP intercept survey data to estimate directed
fishing trips by species, state, mode, and area along the US East Coast.

## Usage

``` r
pull_mrip_directed_trips(
  species = c("ATLANTIC CROAKER", "ATLANTIC MACKEREL", "BLACK SEA BASS",
    "SPANISH MACKEREL", "STRIPED BASS", "SUMMER FLOUNDER", "SCUP", "SPINY DOGFISH",
    "GOOSEFISH", "TILEFISH", "BLUELINE TILEFISH", "BLUEFISH", "GRAY TRIGGERFISH",
    "KING MACKEREL"),
  states = c(23, 33, 25, 44, 9, 10, 24, 34, 36, 51, 37, 45, 13, 121),
  start_year = 2010,
  end_year = 2024,
  trip_type = 1,
  domain = list(wave = list(c(1, 2, 3, 4, 5, 6))),
  data_dir = here::here("data", "MRIP_Index"),
  download = TRUE
)
```

## Arguments

- species:

  Character vector of species common names (uppercase). E.g.,
  `c("STRIPED BASS", "BLUEFISH")`.

- states:

  Integer vector of FIPS-style MRIP state codes. Defaults to East Coast
  states: ME, NH, MA, RI, CT, DE, MD, NJ, NY, VA, NC, SC, GA, and FL
  (east coast).

- start_year:

  Integer. First year of data to retrieve. Default `2010`.

- end_year:

  Integer. Last year of data to retrieve. Default `2024`.

- trip_type:

  Integer scalar (1–5). Trip definition passed to `MRIP.dirtrips()`. `1`
  = primary target (default).

- domain:

  List defining domain strata passed to `MRIP.dirtrips()`. Defaults to
  all six waves: `list(wave = list(c(1:6)))`.

- data_dir:

  Path to the directory where raw MRIP zip files and extracted CSVs will
  be stored. Defaults to `here::here("data", "MRIP_Index")`.

- download:

  Logical. If `TRUE` (default), scrape NMFS and download any missing zip
  files before processing.

- output_path:

  Path for the processed CSV output file. Set to `NULL` to skip writing.
  Defaults to
  `here::here("data", "processed", "mrip_primary_target.csv")`.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
one row per species-domain combination, containing trip estimates,
standard errors, PSEs, and human-readable `state`, `mode`, and `area`
columns. Invisibly also writes a CSV when `output_path` is not `NULL`.

## Details

The function performs three steps:

1.  **Download**: Scrapes the NMFS MRIP CSV directory for `ps_*.zip`
    files and downloads any that are missing locally.

2.  **Organise**: Moves extracted CSVs into `int<YYYY>` sub-folders
    expected by `MRIP.dirtrips()`.

3.  **Estimate**: Calls `MRIP.dirtrips()` once per species, row-binds
    the results, and attaches readable labels for state, fishing mode,
    and area.

Processing all East Coast states over many years can take several hours.

## Examples

``` r
if (FALSE) { # \dontrun{
# Quick run: two species, Mid-Atlantic only
mid_atl <- c(10, 24, 34, 36, 51)

trips <- get_mrip_directed_trips(
  species    = c("STRIPED BASS", "SUMMER FLOUNDER"),
  states     = mid_atl,
  start_year = 2018,
  end_year   = 2022
)

# Full East Coast run (warning: ~4 hours)
trips_all <- get_mrip_directed_trips(
  species = c("BLUEFISH", "BLACK SEA BASS", "SCUP")
)
} # }
```
