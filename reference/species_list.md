# Mid-Atlantic Species

Raw and cleaned common names for species managed by the MAFMC across six
federal data sets.

## Usage

``` r
species_list(source = "all")
```

## Arguments

- source:

  Filter to desired data set. source = "all" returns complete list.
  Options include "nefsc, vtr, observer, landings, mrip, and permits"

## Value

Data frame of landings by species, principal port and state, landings,
live weight, value, latitude and longitude of principal port.

## Examples

``` r
species_list(source = "all")
#> Error in dplyr::mutate(data.frame(comname = c("mackerel, atlantic", "mackerel, chub",     "croaker, atlantic", "striped bass", "black sea bass", "tilefish",     "bluefish", "butterfish", "monkfish / anglerfish / goosefish",     "scup / porgy", "dogfish, spiny", "flounder, summer / fluke",     "tilefish, blueline", "mackerel, spanish", "mackerel, king",     "ocean quahog", "clam, surf", "squid / loligo", "squid / illex")),     clean_name = stringr::str_remove(comname, pattern = " /.*"),     clean_name = SwimmeR::name_reorder(clean_name), clean_name = dplyr::case_when(comname ==         "squid / illex" ~ "northern shortfin squid", comname ==         "squid / loligo" ~ "longfin squid", .default = clean_name),     data_source = "vtr"): ℹ In argument: `clean_name = SwimmeR::name_reorder(clean_name)`.
#> Caused by error in `loadNamespace()`:
#> ! there is no package called ‘SwimmeR’
```
