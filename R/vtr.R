## VTR Data Pull and Clean ##
# path <- "/Users/clovas/Library/CloudStorage/Box-Box/CONFIDENTIAL_GARFO_MAFMC_2025/VTR_and_Dealerdata_by_port_and_species_1964-2024/"

#' @title Pull federal VTR data
#'
#' @description Function to pull and clean Vessel Trip Reports from pre-existing confidential repository.
#'
#' @param proj_path Local path to data file
#' @return Data frame of vessel trip reports; includes year, sub_trip_id, latitude, longitude, port name and state, species caught, weight of kept and discarded catch.
#' @export
#' @examples # not run
#'
pull_vtr <- function(proj_path){
  state_names <- c(
    ME = "Maine",          NH = "New Hampshire", MA = "Massachusetts",
    CT = "Connecticut",    RI = "Rhode Island",  NY = "New York",
    NJ = "New Jersey",     PA = "Pennsylvania",  MD = "Maryland",
    DE = "Delaware",       VA = "Virginia",      NC = "North Carolina",
    SC = "South Carolina", GA = "Georgia",       FL = "Florida"
  )

  # Read in files
  read_vtr <- function(file_path){
    readr::read_csv(file_path, show_col_types = FALSE) |>
      janitor::clean_names() |>
      dplyr::mutate(
        vtrserno = as.character(vtrserno),
        dplyr::across(c(year, sub_trip_id, calc_lat_deg, calc_lat_min, calc_lat_sec, calc_lon_deg, calc_lon_min, calc_lon_sec, calc_inshr_area), as.numeric)
      )
  }

  tmp <- tibble::tibble("file_path" = list.files(proj_path, pattern = ".csv", full.names = TRUE)) |>
    dplyr::mutate("data" = purrr::map(file_path, read_vtr)) |>
    tidyr::unnest(data) |>
    dplyr::select(!file_path)

  # Build coordinates and clean data
  out <- tmp |>
    dplyr::mutate(lat =   (calc_lat_deg + (calc_lat_min/60) + (calc_lat_sec/3600)),
                  lon = -1*(calc_lon_deg + (calc_lon_min/60) +(calc_lon_sec/3600))) |>
    dplyr::mutate(state_full = dplyr::recode(state_abb, !!!state_names)) |>
    dplyr::filter(!is.na(state_abb)) |>
    dplyr::filter(lat > 20 | lon > -80 & lon < -60) |>
    dplyr::select(year, sub_trip_id, trip_type, lat, lon, species_name, port_name, state_abb, state_full, kept, discarded) |>
    dplyr::distinct()

  return(out)

}

## Plot
#' @title Plot vessel trip report data
#'
#' @description Function to plot distributions of kept catch using vessel trip report (VTR) data. Color indicates `leading` and `trailing` 10% of kept catch, with `center`representing 80% of kept catch. Yellow contours represent density. Distributions are cropped to management zones.
#'
#' @param species Default is "all", includes Mid-Atlantic species represented in `speciesshifts::mid_atlantic_species()`
#' @param data Default is "vtr." `pull_vtr` must be run and named "vtr" in order to run this function.
#' @return Map of distribution of observed kept catch along the Northeast US. Selecting `all` species will return a list.
#' @export
#' @examples # map_vtr(species = "summer flounder", data = "vtr")
#'
map_vtr <- function(species = "all", data = "vtr"){

  # Get species list
  species_list <- speciesshifts::mid_atlantic_species(source = "vtr")

  # Base filter
  data <- vtr |>
    dplyr::mutate(comname = tolower(species_name),
                  decade  = 10*year%/%10) |>
    dplyr::right_join(species_list)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$clean_name) {
      message("Species '", species, "' not found.")
      return(NULL)
    }
    data <- data |> dplyr::filter(clean_name == species)
  }

  # Spatial goodies
  sf::sf_use_s2(FALSE)

  shp_path <- here::here("data", "shapefiles", "Council_Scopes.shp")

  boundaries <- sf::st_read(shp_path, quiet = TRUE)
  boundaries <- ggplot2::fortify(boundaries)

  east_coast <- boundaries |>
    janitor::clean_names() |>
    dplyr::filter(council %in% c("New England", "Mid-Atlantic", "South Atlantic")) |>
    dplyr::mutate(factor = factor(council, levels = c("New England", "Mid-Atlantic", "South Atlantic")))

  usa <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")
  can <- rnaturalearth::ne_states(country = "canada", returnclass = "sf")

  # Crop data
  sf <- data |>
    sf::st_as_sf(coords = c("lon","lat"),
                 crs = sf::st_crs(east_coast))

  crop <- sf |>
    sf::st_join(east_coast,
                join = sf::st_intersects)
  data <- crop |>
    cbind(sf::st_coordinates(crop)) |> # convoluted but whatever
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(council)) |>
    dplyr::rename("lon" = "X",
                  "lat" = "Y") |>
    dplyr::select(year, decade, sub_trip_id, trip_type, species_name, clean_name, lat, lon, kept, discarded, port_name, state_abb, state_full, council)

  # Percentiles by decade
  tmp <- data |>
    dplyr::group_by(clean_name, decade) |>
    dplyr::summarise(`5%` = Hmisc::wtd.quantile(lat,  w = kept, probs = 0.05),
                     `95%` = Hmisc::wtd.quantile(lat, w = kept, probs = 0.95))

  # Join together and plot
  plots <- data |>
    dplyr::right_join(tmp, by = dplyr::join_by(clean_name, decade)) |>
    dplyr::group_by(clean_name, decade) |>
    dplyr::mutate(partition =
                    dplyr::case_when(
                      lat <= `5%` ~ "Trailing",
                      lat > `5%` & lat < `95%` ~ "Center",
                      lat >= `95%` ~ "Leading"
    )) |>
    dplyr::mutate(partition = factor(partition, levels = c("Trailing", "Center", "Leading"))) |>
    dplyr::group_by(clean_name) |>
    tidyr::nest() |>
    dplyr::mutate(
      out = purrr::map(data, function(x){
        ggplot2::ggplot() +
          ggplot2::geom_sf(data = usa) +
          ggplot2::geom_sf(data = can) +
          ggplot2::geom_sf(data = east_coast, fill = "transparent") +
          ggplot2::coord_sf(ylim = c(35,45), xlim = c(-66,-78)) +
          ggplot2::scale_x_continuous(breaks = c(-76, -72, -68)) +
          ggplot2::geom_point(data = x,
            ggplot2::aes(x = lon, y = lat, color = partition, group = partition, alpha = kept)
          ) +
          ggplot2::stat_density2d(data = x,
            ggplot2::aes(x = lon, y = lat), alpha = 0.8, color = "#ebcb27") +
          ggplot2::scale_color_manual(values = c("#363b45", "#00608a","#C1DEFF")) +
          ggplot2::facet_wrap(
            ~stringr::str_to_title(decade),
            nrow = 1
          ) +
          ggplot2::guides(
            color = ggplot2::guide_legend(title = "Fleet distibution"),
            alpha = ggplot2::guide_legend(title = "Kept catch")
          ) +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 12),
            axis.title        = ggplot2::element_blank(),
            legend.position   = "bottom",
            legend.box        = "vertical",
            strip.background  = ggplot2::element_blank(),
            strip.text        = ggplot2::element_text(hjust = 0, face = "bold", size = 12),
            panel.grid.major  = ggplot2::element_line(color = "#535353", linewidth = 0.1, linetype = 3),
            panel.grid.minor  = ggplot2::element_blank(),
            panel.background  = ggplot2::element_rect(fill = "transparent"),
            panel.border      = ggplot2::element_rect(
              fill      = "transparent",
              linetype  = 1,
              linewidth = 0.5,
              color     = "#535353"
              ))
     }))
  if (species == "all") {
    return(plots |> dplyr::select(clean_name, out))
  } else {
    return(plots$out[[1]])
  }
}

