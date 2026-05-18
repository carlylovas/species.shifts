# Pull NEFSC survdat data

Function to pull and clean NEFSC Spring-Fall Bottom Trawl Survey
(survdat) data from pre-existing confidential repository.

## Usage

``` r
pull_nefsc(proj_path)
```

## Arguments

- proj_path:

  Local path to data file

## Value

Data frame of observer data; contains both catch and haul information.

## Examples

``` r
# nefsc <- pull_nefsc(proj_path = "~Data/trawl_dat.rds")
```
