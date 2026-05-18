# Plotting permit 5th, 50th, and 95th weighted percentiles of latitudinal distribtion.

Function to plot percentiles of permits across states for Mid-Atlantic
species.

## Usage

``` r
plot_permit_edges(species = "all", data = "permits")
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
# plot_permit_edges(species = "summer flounder", data = permits)
```
