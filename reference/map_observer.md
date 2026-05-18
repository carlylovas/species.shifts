# Plot observer data

Function to plot distributions of kept catch using federal observer
data. Color indicates \`leading\` and \`trailing\` 10

## Usage

``` r
map_observer(species = "all", data = "observer")
```

## Arguments

- species:

  Default is "all", includes Mid-Atlantic species represented in
  \`species.shifts::species_list()\`

- data:

  Default is "observer" \`pull_observer\` must be run and named
  "observer" in order to run this function.

## Value

Map of distribution of observed catch along the Northeast US. Selecting
\`all\` species will return a list.

## Examples

``` r
# map_observer(species = "summer flounder", data = "observer")
```
