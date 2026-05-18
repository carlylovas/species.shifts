# Plot vessel trip report data

Function to plot distributions of kept catch using vessel trip report
(VTR) data. Color indicates \`leading\` and \`trailing\` 10

## Usage

``` r
map_vtr(species = "all", data = "vtr")
```

## Arguments

- species:

  Default is "all", includes Mid-Atlantic species represented in
  \`species.shifts::species_list()\`

- data:

  Default is "vtr." \`pull_vtr\` must be run and named "vtr" in order to
  run this function.

## Value

Map of distribution of observed kept catch along the Northeast US.
Selecting \`all\` species will return a list.

## Examples

``` r
# map_vtr(species = "summer flounder", data = "vtr")
```
