# Plotting permit proportions by Management Councils.

Function to plot proportions of permits across states for Mid-Atlantic
species.

## Usage

``` r
plot_council_permits(species = "all", data = "permits")
```

## Arguments

- species:

  Mid-Atlantic managed species as listed in \`species_list(source =
  "permits")\`

- data:

  Landings outputs from \`pull_permits()\`

## Value

List of faceted plots.

## Examples

``` r
# plot_council_permits(species = "summer flounder", data = permits)
```
