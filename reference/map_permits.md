# Map permits by type along NEUS coast.

Function to plot distributions of permits along the Northeast US
coastline.

## Usage

``` r
map_permits(species = "all", data = "permits")
```

## Arguments

- species:

  Default is "all", includes Mid-Atlantic species represented in
  \`species.shifts::species_list()\`

- data:

  Default is "permits" \`pull_permits\` must be run and named "permits"
  in order to run this function.

## Value

Map of distribution of observed kept catch along the Northeast US.
Selecting \`all\` species will return a list.

## Examples

``` r
# map_permits(species = "summer flounder", data = "permits")
```
