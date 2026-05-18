# Plot NEFSC Bottom Trawl Center of Biomass

Function to calculate the annual centers of biomass by season of
Mid-Atlantic species and plot latitude and the distance between them.

## Usage

``` r
plot_nefsc_centers(species = "all", data = "nefsc")
```

## Arguments

- species:

  Default is "all", includes Mid-Atlantic species represented in
  \`species.shift::species_list()\`

- data:

  Default is "nefsc". \`pull_nefsc()\` must be run prior in order to run
  this function.

## Value

Plot of fall and spring centers of latitude. Selecting \`all\` species
will return a list.

## Examples

``` r
# plot_nefsc_centers(species = "black sea bass", data = nefsc)
```
