# Map NEFSC Bottom Trawl Center of Biomass

Function to calculate the seasonal centers of biomass of Mid-Atlantic
species and map them.

## Usage

``` r
map_nefsc_cob(species = "all", data = "nefsc")
```

## Arguments

- species:

  Default is "all", includes Mid-Atlantic species represented in
  \`species.shift::species_list()\`

- data:

  Default is "nefsc". \`pull_nefsc()\` must be run prior in order to run
  this function.

## Value

Map of distribution of seasonal centers of biomass along the Northeast
US. Selecting \`all\` species will return a list.

## Examples

``` r
# map_nefsc_cob(species = "black sea bass", data = nefsc)
```
