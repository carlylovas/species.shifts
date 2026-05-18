# Pull federal landings data

Function to pull GARFO Dealer data and geocode principal ports from
pre-existing repository.

## Usage

``` r
pull_garfo_landings(proj_path = NULL, sheet = NULL, skip = NULL)
```

## Arguments

- proj_path:

  Local path to data file

- sheet:

  Derived from readxl::read_xlsx(). Indiciates which sheet of Excel file
  to read.

- skip:

  Also derived from readxl::read_xlsx(). Indicates the number of rows to
  skip, if any.

## Value

Data frame of landings by species, principal port and state, landings,
live weight, value, latitude and longitude of principal port.

## Examples

``` r
# my_path <- "/Users/clovas/Library/CloudStorage/Box-Box/CONFIDENTIAL_GARFO_MAFMC_2025/VTR_and_Dealerdata_by_port_and_species_1964-2024/KMills_VTR Data Dump & Dealer by Port_JUN 2025.xlsx"
```
