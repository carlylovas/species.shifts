# Map NEFSC biomass data

Function to maps distributions of biomass in NEFSC Spring-Fall Bottom
Trawl Survey. Color indicates \`leading\` and \`trailing\` 10

## Usage

``` r
map_nefsc(species = "all", data = "nefsc")
```

## Arguments

- species:

  Default is "all", includes Mid-Atlantic species represented in
  \`species.shift::species_list()\`

- data:

  Default is "nefsc" \`nefsc\` must be run and named "observer" in order
  to run this function.

## Value

Map of distribution of biomass along the Northeast US. Selecting \`all\`
species will return a list.

## Examples

``` r
# map_nefsc(species = "summer flounder", data = "nefsc")
```
