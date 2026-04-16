# Pull GARFO Dealer Data from confidential repository and clean ####

# Data pull ----

#' @title Pull federal landings data
#'
#' @description Function to pull GARFO Dealer data and geocode principal ports from pre-existing repository.
#'
#' @param proj_path Local path to data file
#' @param sheet Derived from readxl::read_xlsx(). Indiciates which sheet of Excel file to read.
#' @param skip Also derived from readxl::read_xlsx(). Indicates the number of rows to skip, if any.
#' @return Data frame of landings by species, principal port and state, landings, live weight, value, latitude and longitude of principal port.
#' @export
#'
#' @examples # my_path <- "/Users/clovas/Library/CloudStorage/Box-Box/CONFIDENTIAL_GARFO_MAFMC_2025/VTR_and_Dealerdata_by_port_and_species_1964-2024/KMills_VTR Data Dump & Dealer by Port_JUN 2025.xlsx"

pull_garfo_landings <- function(proj_path = NULL, sheet = NULL, skip = NULL) {

  state_names <- c(
    ME = "Maine",          NH = "New Hampshire", MA = "Massachusetts",
    CT = "Connecticut",    RI = "Rhode Island",  NY = "New York",
    NJ = "New Jersey",     PA = "Pennsylvania",  MD = "Maryland",
    DE = "Delaware",       VA = "Virginia",      NC = "North Carolina",
    SC = "South Carolina", GA = "Georgia",       FL = "Florida"
  )

  council_map <- c(
    "Maine" = "North Atlantic",         "New Hampshire" = "North Atlantic",
    "Massachusetts" = "North Atlantic", "Connecticut" = "North Atlantic",
    "Rhode Island" = "North Atlantic",  "New York" = "Mid-Atlantic",
    "New Jersey" = "Mid-Atlantic",      "Pennsylvania" = "Mid-Atlantic",
    "Maryland" = "Mid-Atlantic",        "Delaware" = "Mid-Atlantic",
    "Virginia" = "Mid-Atlantic",        "North Carolina" = "South Atlantic",
    "South Carolina" = "South Atlantic","Georgia" = "South Atlantic",
    "Florida" = "South Atlantic"
  )

  tmp <- readxl::read_xlsx(path = proj_path, sheet = sheet, skip = skip) |>
    dplyr::filter(YEAR >= 1996) |>
    janitor::clean_names() |>
    dplyr::mutate(
      species_name = tolower(species_name),
      comname      = SwimmeR::name_reorder(species_name)
    )

  geocodes <- tmp |>
      dplyr::select(portnm, state) |>
      dplyr::distinct() |>
      tidygeocoder::geocode(city = "portnm", state = "state")

  tmp |>
      dplyr::left_join(geocodes) |>
      dplyr::mutate(
        state_full = dplyr::recode(state, !!!state_names),
        council    = factor(council_map[state_full], levels = c("New England", "Mid-Atlantic", "South Atlantic"))
      )
}

# landings <- pull_garfo_landings(proj_path = my_path, sheet = 1, skip = 9)

# Plotting functions ----

#' @title Plotting landings trends
#'
#' @description Function to plot landed value and volume trends for Mid-Atlantic species.
#'
#' @param data Landings outputs from `pull_garfo_landings()`
#' @param species Mid-Atlantic managed species as listed in `mid_atlantic_species(source = "landings")`
#' @return List of faceted plots.
#' @import ggplot2
#' @export
#'
#' @examples # not run

## Landings trends
landings_trends <- function(species = "all", data = "landings") {

  # Get species list
  species_list <- speciesshifts::mid_atlantic_species(source = "landings")

  # Base filter
  data <- landings |>
    dplyr::filter(comname %in% species_list$comname)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$comname) {
      message("Species '", species, "' not found in landings data.")
      return(NULL)
    }
    data <- data |> dplyr::filter(comname == species)
  }

  # Build plots only for relevant species
  plots <- data |>
    dplyr::rename("landings" = "land") |>
    tidyr::pivot_longer(
      cols = c(landings, value),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::group_by(year, comname, state_full, metric) |>
    dplyr::summarise(total = sum(value), .groups = "drop") |>
    dplyr::group_by(comname) |>
    tidyr::nest() |>
    dplyr::mutate(
      out = purrr::map2(data, comname, function(x, y) {
        ggplot2::ggplot(data = x) +
          ggplot2::geom_line(
            ggplot2::aes(x = year, y = total, color = state_full)
          ) +
          gmRi::scale_color_gmri() +
          ggplot2::facet_wrap(
            ~stringr::str_to_title(metric),
            nrow = 1,
            scales = "free_y"
          ) +
          ggplot2::guides(
            col = ggplot2::guide_legend(nrow = 2)
          ) +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 12),
            legend.title      = ggplot2::element_blank(),
            axis.title        = ggplot2::element_blank(),
            legend.position   = "bottom",
            strip.background  = ggplot2::element_blank(),
            strip.text        = ggplot2::element_text(hjust = 0, face = "plain", size = 15),
            panel.grid.major  = ggplot2::element_line(color = "#535353", linewidth = 0.1, linetype = 3),
            panel.grid.minor  = ggplot2::element_blank(),
            panel.background  = ggplot2::element_rect(fill = "transparent"),
            panel.border      = ggplot2::element_rect(
              fill      = "transparent",
              linetype  = 1,
              linewidth = 0.5,
              color     = "#535353"
            )
          )
      })
    ) |>
    dplyr::select(!data)

  return(plots$out)
}

## State proportions
#' @title Plotting landings proportions by state
#'
#' @description Function to plot proportions of landings across states for Mid-Atlantic species.
#'
#' @param data Landings outputs from `pull_garfo_landings()`
#' @param species Mid-Atlantic managed species as listed in `mid_atlantic_species(source = "landings")`
#' @return List of faceted plots.
#' @import ggplot2
#' @export
#'
#' @examples # not run

state_proportions <- function(species = "all", data = "landings") {

  # Get species list
  species_list <- speciesshifts::mid_atlantic_species(source = "landings")

  # Base filter
  data <- landings |>
    dplyr::filter(comname %in% species_list$comname)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$comname) {
      message("Species '", species, "' not found in landings data.")
      return(NULL)
    }
    data <- data |> dplyr::filter(comname == species)
  }

  # Build plots only for relevant species
  plots <- data |>
    dplyr::rename("landings" = "land") |>
    tidyr::pivot_longer(
      cols = c(landings, value),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::group_by(year, comname, state_full, metric) |>
    dplyr::summarise(state_value = sum(value, na.rm = T), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::group_by(year, comname, metric) |>
    dplyr::mutate(total_value = sum(state_value, na.rm = T),
                  prop = state_value / total_value) |>
    dplyr::group_by(comname) |>
    tidyr::nest() |>
    dplyr::mutate(
      out = purrr::map2(data, comname, function(x, y) {
        ggplot2::ggplot(data = x) +
          ggplot2::geom_col(
            ggplot2::aes(x = year, y = prop, fill = state_full)
          ) +
          gmRi::scale_fill_gmri() +
          ggplot2::facet_wrap(
            ~stringr::str_to_title(metric),
            nrow = 1,
            scales = "free_y"
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(nrow = 2)
          ) +
          ggplot2::labs(
            title = "Proportion of landings by state",
            x = "Year",
            y = "Proportion"
          ) +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir"),
            legend.title      = ggplot2::element_blank(),
            axis.title        = ggplot2::element_blank(),
            legend.position   = "bottom",
            strip.background  = ggplot2::element_blank(),
            strip.text        = ggplot2::element_text(hjust = 0, face = "plain", size = 15),
            panel.grid.major  = ggplot2::element_line(color = "#535353", linewidth = 0.1, linetype = 3),
            panel.grid.minor  = ggplot2::element_blank(),
            panel.background  = ggplot2::element_rect(fill = "transparent"),
            panel.border      = ggplot2::element_rect(
              fill      = "transparent",
              linetype  = 1,
              linewidth = 0.5,
              color     = "#535353"
            )
          )
      })
    ) |>
    dplyr::select(!data)

  return(plots$out)
}

## Council proportions
#' @title Plotting landings proportions by ASMFC Council Management Zones
#'
#' @description Function to plot proportions of landings across Council Management Zones for Mid-Atlantic species.
#'
#' @param data Landings outputs from `pull_garfo_landings()`
#' @param species Mid-Atlantic managed species as listed in `mid_atlantic_species(source = "landings")`
#' @return List of faceted plots.
#' @import ggplot2
#' @export
#'
#' @examples # not run

council_proportions <- function(species = "all", data = "landings") {

  # Get species list
  species_list <- speciesshifts::mid_atlantic_species(source = "landings")

  # Base filter
  data <- landings |>
    dplyr::filter(comname %in% species_list$comname)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$comname) {
      message("Species '", species, "' not found in landings data.")
      return(NULL)
    }
    data <- data |> dplyr::filter(comname == species)
  }

  # Build plots only for relevant species
  plots <- data |>
    dplyr::rename("landings" = "land") |>
    tidyr::pivot_longer(
      cols = c(landings, value),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::group_by(year, comname, council, metric) |>
    dplyr::summarise(council_value = sum(value, na.rm = T), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::group_by(year, comname, metric) |>
    dplyr::mutate(total_value = sum(council_value, na.rm = T),
                  prop = council_value / total_value) |>
    dplyr::group_by(comname) |>
    tidyr::nest() |>
    dplyr::mutate(
      out = purrr::map2(data, comname, function(x, y) {
        ggplot2::ggplot(data = x) +
          ggplot2::geom_col(
            ggplot2::aes(x = year, y = prop, fill = council)
          ) +
          ggplot2::scale_fill_manual(values = c("#363b45", "#00608a","#C1DEFF")) +
          ggplot2::facet_wrap(
            ~stringr::str_to_title(metric),
            nrow = 1,
            scales = "free_y"
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(nrow = 1)
          ) +
          ggplot2::labs(
            title = "Proportion of landings by Council",
            x = "Year",
            y = "Proportion"
          ) +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 12),
            legend.title      = ggplot2::element_blank(),
            axis.title        = ggplot2::element_blank(),
            legend.position   = "bottom",
            strip.background  = ggplot2::element_blank(),
            strip.text        = ggplot2::element_text(hjust = 0, face = "plain", size = 15),
            panel.grid.major  = ggplot2::element_line(color = "#535353", linewidth = 0.1, linetype = 3),
            panel.grid.minor  = ggplot2::element_blank(),
            panel.background  = ggplot2::element_rect(fill = "transparent"),
            panel.border      = ggplot2::element_rect(
              fill      = "transparent",
              linetype  = 1,
              linewidth = 0.5,
              color     = "#535353"
            )
          )
      })
    ) |>
    dplyr::select(!data)

  return(plots$out)
}
