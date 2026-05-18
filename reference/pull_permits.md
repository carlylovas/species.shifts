# Pull federal permits data

Function to pull, clean and geocode GARFO commercial fishing permits
from pre-existing confidential repository. Note that due to spelling
error, geocoding principal ports removes 1

## Usage

``` r
pull_permits(proj_path)
```

## Arguments

- proj_path:

  Local path to data file

## Value

Data frame of permits; includes year, prinicpal port and state, permit
type, target species, and category (commerical, for-hire).

## Examples

``` r
# permits <- pull_permits(proj_path = proj_path)
```
