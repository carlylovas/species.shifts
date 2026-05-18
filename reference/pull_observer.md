# Pull federal observer data

Function to pull and clean NOAA Fisheries Observer data from
pre-existing confidential repository.

## Usage

``` r
pull_observer(proj_path)
```

## Arguments

- proj_path:

  Local path to data file

## Value

Data frame of observer data; contains both catch and haul information.

## Examples

``` r
# observer <- pull_observer(proj_path = "~Documents/obs_dat.xlsx")
```
