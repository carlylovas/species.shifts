# Plotting permit proportions by state

Function to plot proportions of landings across states for Mid-Atlantic
species.

## Usage

``` r
plot_state_permits(species = "all", data = "permits")
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
# plot_state_permits(species = "summer flounder", data = permits)
```
