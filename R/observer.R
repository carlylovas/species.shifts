## File paths ----
# box_path  <- "/Users/clovas/Library/CloudStorage/Box-Box/MAFMC-25 Data/"
# proj_path <- paste0(box_path, "Confidential/Observer/DR25-200_Mills_Allyn.xlsx")

## Data ###################################
#' @title Pull federal observer data
#'
#' @description Function to pull and clean NOAA Fisheries Observer data from pre-existing confidential repository.
#'
#' @param proj_path Local path to data file
#' @return Data frame of observer data; contains both catch and haul information.
#' @export
#' @examples # observer <- pull_observer(proj_path = "~Documents/obs_dat.xlsx")
#'
pull_observer <- function(proj_path){

  # Read in data (excel spreadsheet) ----
  data <- proj_path |>
    readxl::excel_sheets() |>
    rlang::set_names()  |>
    purrr:::map(
            readxl::read_excel, path = proj_path)

  haul_data <- tibble::tibble(
    rbind(data$haul_1989_2009, data$haul_2010_2024)) |>
    janitor::clean_names()

  catch <- data[stringr::str_detect(names(data), "catch_")]

  ## 1989-1995 missing a column for year, using LINK1 to extract year column

  catch_samp <- tibble::tibble(
    data.table::rbindlist(
      catch[c(1:7)])
    ) |>
    dplyr::mutate(YEAR = as.numeric(
      stringr::str_sub(
        LINK1, start = 4, end = -9)
      ),
         SPECIES_ITIS = as.numeric(SPECIES_ITIS),
         LIVE_WT = as.numeric(LIVE_WT))

  catch_data <- tibble::tibble(
    data.table::rbindlist(
      catch[c(8:36)])
    ) |> # this is every year onward
    dplyr::full_join(catch_samp) |>
    dplyr::arrange(YEAR) |>
    janitor::clean_names()

  # Cleaning ----
  haul_data |>
    dplyr::select(link1, link3, negear, targspec1, targspec2, targspec3, gis_lathbeg, gis_lonhbeg, gis_latsbeg, gis_lonsbeg) |>
    dplyr::group_by(link3) |>
    dplyr::full_join(catch_data) |>
    dplyr::relocate(year, .after = link3) |>
    dplyr::relocate(comname, .after = year) |>
    dplyr::mutate(kept = ifelse(stringr::str_detect(fishdispdesc, "KEPT"), "Kept", "Discarded")) |>
    dplyr::select(link3, year, negear, comname, targspec1, targspec2, targspec3, gis_lathbeg, gis_lonhbeg, gis_latsbeg, gis_lonsbeg, hailwt, live_wt, kept) |>
    dplyr::filter(!is.na(live_wt)) -> catch_haul

  catch_haul |>
    dplyr::filter(!is.na(gis_latsbeg) & !gis_latsbeg == 1 & (is.na(gis_lathbeg))) |>
    dplyr::select(!c(gis_lathbeg, gis_lonhbeg)) |>
    dplyr::rename(lat = gis_latsbeg,
                  lon = gis_lonsbeg) -> set_lats

  catch_haul |>
    dplyr::filter(!link3 %in% set_lats$link3) |>
    dplyr::select(!c(gis_latsbeg, gis_lonsbeg)) |>
    dplyr::rename(lat = gis_lathbeg,
           lon = gis_lonhbeg) |>
    dplyr::full_join(set_lats) |>
    tidyr::drop_na() |>
    dplyr::mutate(decade =  10*year%/%10) -> catch_coords

  return(catch_coords)

}

# observer <- pull_observer(proj_path = proj_path)

## Plot ###################################
#' @title Plot observer data
#'
#' @description Function to plot distributions of kept catch using federal observer data. Color indicates `leading` and `trailing` 10% of kept catch, with `center`representing 80% of kept catch. Yellow contours represent density. Distributions are cropped to management zones.
#'
#' @param species Default is "all", includes Mid-Atlantic species represented in `speciesshifts::mid_atlantic_species()`
#' @param data Default is "observer" `pull_observer` must be run and named "observer" in order to run this function.
#' @return Map of distribution of observed catch along the Northeast US. Selecting `all` species will return a list.
#' @export
#' @examples # map_observer(species = "summer flounder", data = "observer")
#'
map_observer <- function(species = "all", data = "observer"){

  # Get species list
  species_list <- speciesshifts::mid_atlantic_species(source = "observer")

  # Base filter
  data <- data |>
    dplyr::mutate(comname = tolower(comname)) |>
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
                  "lat" = "Y")

  # Percentiles by decade
  tmp <- data |>
    dplyr::group_by(clean_name, decade) |>
    dplyr::summarise(`5%` = Hmisc::wtd.quantile(lat,  w = live_wt, probs = 0.05),
                     `95%` = Hmisc::wtd.quantile(lat, w = live_wt, probs = 0.95))

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
                              ggplot2::aes(x = lon, y = lat, color = partition, group = partition, alpha = live_wt)
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
            alpha = ggplot2::guide_legend(title = "Observed catch")
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
