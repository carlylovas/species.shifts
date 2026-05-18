# Accessing and cleaning federal fisheries data

## About

This package contains various functions for accessing (pulling) and
cleaning federal fisheries dependent and independent data. Many of these
data sets are confidential and require a formal data request submission
to the appropriate governing body. Information regarding metadata and
access for most of these data sets can be accessed via
[inPort](https://www.fisheries.noaa.gov/inport/), see below:

> InPort is the authoritative metadata repository and data inventory
> platform for NOAA Fisheries and the National Ocean Service. The system
> supports documentation of datasets and provides tools to facilitate
> data discovery, public access, and responsible stewardship of
> scientific data within these line offices.

As such, using `species.shifts` will require access to this data in
order to use the cleaning and plotting functions. It is *strongly*
recommended that this data be stored locally in a centralized
repository. Please refer to the package `README` more information on
recommended work flows.

## Data sets

The functions in this package correspond to six federal fisheries data
sets, each with its own respective `pull_()` function.

[NOAA Fisheries Vessel Trip
Reports](https://www.fisheries.noaa.gov/inport/item/11489)

[NEFSC Observer at
Sea](https://www.fisheries.noaa.gov/inport/item/24111)

[NEFSC Spring-Fall Bottom Trawl
Survey](https://www.fisheries.noaa.gov/inport/item/22557)

[GARFO Permits Data](https://apps-garfo.fisheries.noaa.gov/permits/)

[GARFO Dealer Reported
Landings](https://www.fisheries.noaa.gov/contact/greater-atlantic-regional-fisheries-office)

[NOAA Fisheries Marine Recreational Information
Program](https://www.fisheries.noaa.gov/insight/marine-recreational-information-program)

------------------------------------------------------------------------

### Vessel trip reports

`vtr <- pull_vtr(proj_path = my_path)`

    Error in `dplyr::mutate()`:
    ℹ In argument: `lat = (calc_lat_deg + (calc_lat_min/60) +
      (calc_lat_sec/3600))`.
    Caused by error:
    ! object 'calc_lat_deg' not found

\*Trip ID information removed from demo to maintain confidentiality.

### Fisheries Observer

`observer <- pull_observer(proj_path = my_path)`

    Error:
    ! `path` does not exist: '/Users/clovas/Library/CloudStorage/Box-Box/MAFMC-25 Data/Confidential/Observer/DR25-200_Mills_Allyn.xlsx'

\*Trip ID information removed from demo to maintain confidentiality.

### NEFSC Bottom Trawl

`nefsc <- pull_nefsc(proj_path = my_path)`

    Error in `readRDS()`:
    ! cannot open the connection

### GARFO Federal Permits

`permits <- pull_permits(proj_path = my_path)`

    Error in `loadNamespace()`:
    ! there is no package called 'here'

### GARFO Dealer-reported landings

`landings <- pull_landings(proj_path = my_path)`

    Error:
    ! `path` does not exist: '/Users/clovas/Library/CloudStorage/Box-Box/MAFMC-25 Data/Confidential/VTR and Dealer/Dealer Data 2025.xlsx'

### Marine Recreational Information Program

#### Directed trips

`mrip_directed_trips <- pull_mrip_directed_trips()`

    Error in `loadNamespace()`:
    ! there is no package called 'here'

#### Catch estimates

`mrip_catch <- pull_mrip_catch()`

    Error:
    ! '/Users/clovas/Library/CloudStorage/Box-Box/MAFMC-25
      Data/Non-confidential/ACCSP/Catch_Estimates.csv' does not exist.

## Plotting

Once the data has been pulled and saved to the local environment, each
data set has a respective `plot_()` or `map_()` function. More about
those functions
[here](https://carlylovas.github.io/species.shifts/vignettes/plotting_functions.html).
