## NEFSC Spring-Fall Bottom Trawl Survey ##
### Data pull, clean and plot ###
# box_path  <- "/Users/clovas/Library/CloudStorage/Box-Box/MAFMC-25 Data/"
# proj_path <- paste0(box_path, "Non-confidential/NEFSC Trawl/survdat_lw.rds")


## Data ###################################
#' @title Pull NEFSC survdat data
#'
#' @description Function to pull and clean NEFSC Spring-Fall Bottom Trawl Survey (survdat) data from pre-existing confidential repository.
#'
#' @param proj_path Local path to data file
#' @return Data frame of observer data; contains both catch and haul information.
#' @export
#' @examples # nefsc <- pull_nefsc(proj_path = "~Data/trawl_dat.rds")

# Load and preliminary cleaning of raw data ----
pull_nefsc <- function(proj_path){

  survdat <- readr::read_rds(proj_path)$survdat |>
    as.data.frame()

  # Some clean up
  trawldat <- janitor::clean_names(survdat)

  # Add in species common name
  spp_classes <- readr::read_csv(here::here("data","sppclass.csv"), # Make sure this is in there
                                 col_types = readr::cols()
  )
  spp_classes <- janitor::clean_names(spp_classes)

  # Fixing lobster
  spp_classes$scientific_name[which(spp_classes$svspp == 301)] <- "Homarus americanus"

  spp_classes <- dplyr::mutate(
    .data = spp_classes, comname = stringr::str_to_lower(common_name),
    scientific_name = stringr::str_to_lower(scientific_name)
  )
  spp_classes <- dplyr::distinct(spp_classes, svspp, comname, scientific_name)
  trawldat <- dplyr::mutate(trawldat, svspp = stringr::str_pad(svspp, 3, "left", "0"))
  trawldat <- dplyr::left_join(trawldat, spp_classes, by = "svspp")

  # Creating a unique tow ID column
  trawldat <- dplyr::mutate(.data = trawldat, cruise6 = stringr::str_pad(
    cruise6,
    6, "left", "0"
  ), station = stringr::str_pad(
    station,
    3, "left", "0"
  ), stratum = stringr::str_pad(
    stratum,
    4, "left", "0"
  ), id = stringr::str_c(
    cruise6, station,
    stratum
  ))

  # Adding a date column
  trawldat <- dplyr::mutate(.data = trawldat, est_month = stringr::str_sub(
    est_towdate,
    6, 7
  ), est_month = as.numeric(est_month), est_day = stringr::str_sub(
    est_towdate,
    -2, -1
  ), est_day = as.numeric(est_day), .before = season)

  # Column names/formatting
  trawldat <- dplyr::mutate(.data = trawldat, comname = tolower(comname), id = format(id, scientific = FALSE), svspp = as.character(svspp), svspp = stringr::str_pad(svspp, 3, "left", "0"), season = stringr::str_to_title(season), strat_num = stringr::str_sub(stratum, 2, 3))
  trawldat <- dplyr::rename(.data = trawldat, biomass_kg = biomass, length_cm = length)

  # Dealing with when there is biomass/no abundance, or abundance but no biomass
  trawldat <- dplyr::mutate(.data = trawldat, biomass_kg = ifelse(biomass_kg == 0 & abundance > 0, 1e-04, biomass_kg), abundance = ifelse(abundance == 0 & biomass_kg > 0, 1, abundance))
  trawldat <- dplyr::filter(.data = trawldat, !is.na(biomass_kg), !is.na(abundance))

  # Filtering strata not regularly sampled throughout the time series
  trawldat <- dplyr::filter(.data = trawldat, stratum >= 1010, stratum <= 1760, stratum != 1310, stratum != 1320, stratum != 1330, stratum != 1350, stratum != 1410, stratum != 1420, stratum != 1490)

  # Filtering species not regularly sampled (shrimps, others?)
  trawldat <- dplyr::filter(.data = trawldat, !svspp %in% c(285:299, 305, 306, 307, 316, 323, 910:915, 955:961))
  trawldat <- dplyr::filter(trawldat, !svspp %in% c(0, "000", 978, 979, 980, 998))

  trawldat <- dplyr::filter(trawldat, year >= 1970)

  # Getting distinct biomass values at the species level
  dat_clean <- trawldat |>
    dplyr::distinct(id, svspp, catchsex, comname, year, est_month, est_day, season, lat, lon, est_towdate, biomass_kg) |>
    dplyr::group_by(id, svspp, comname, year, est_month, est_day, season, lat, lon, est_towdate) |>
    dplyr::summarize("total_biomass_kg" = sum(biomass_kg)) |>
    dplyr::ungroup()
}

# nefsc <- pull_nefsc(proj_path = proj_path)

## Plots ###################################
## Maps
#' @title Map NEFSC Bottom Trawl Center of Biomass
#'
#' @description Function to calculate the seasonal centers of biomass of Mid-Atlantic species and map them.
#'
#' @param species Default is "all", includes Mid-Atlantic species represented in `species.shift::species_list()`
#' @param data Default is "nefsc". `pull_nefsc()` must be run prior in order to run this function.
#' @return Map of distribution of seasonal centers of biomass along the Northeast US. Selecting `all` species will return a list.
#' @export
#' @examples # map_nefsc_cob(species = "black sea bass", data = nefsc)

map_nefsc_cob <- function(species = "all", data = "nefsc"){

  # Get species list
  species_list <- species.shift::species_list(source = "nefsc")

  # Base filter
  data <- data |>
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

  # Calculate center of biomass and plot
  plots <- data |>
    dplyr::group_by(clean_name, year, season) |>
    dplyr::reframe(
      lat = matrixStats::weightedMean(x = lat, w = total_biomass_kg),
      lon = matrixStats::weightedMean(x = lon, w = total_biomass_kg),
      decade = 10*year%/%10
    ) |>
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
                              ggplot2::aes(x = lon, y = lat, color = season)
          ) +
          gmRi::scale_color_gmri() +
          ggplot2::labs(
            title = "Center of Biomass",
            subtitle = "Biomass-weighted average latitude and longitude"
          ) +
          ggplot2::facet_wrap(
            ~stringr::str_to_title(decade),
            nrow = 1
          ) +
          ggplot2::guides(
            color = ggplot2::guide_legend(title = "Season")
          ) +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 12),
            plot.title        = ggplot2::element_text(face = "bold"),
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

## Seasonal percentile plots
#' @title Plot edges of distribution
#'
#' @description Function to calculate and plot the 5 year rolling means of the 5ht, 50th, and 95th percentiles of biomass-weighted latitude in the NEFSC Bottom Trawl Survey. Plots spring and fall independently.
#'
#' @param species Default is "all", includes Mid-Atlantic species represented in `species.shift::species_list()`
#' @param data Default is "nefsc". `pull_nefsc()` must be run prior in order to run this function.
#' @return Faceted plot of spring and fall percentiles of distributions. Plots both rolling mean values and smoothed values. Selecting `all` species will return a list.
#' @export
#' @examples # plot_nefsc_edges(species = "summer flounder", data = nefsc)

plot_nefsc_edges <- function(species = "all", data = "nefsc"){

  # Get species list
  species_list <- species.shift::species_list(source = "nefsc")

  # Base filter
  data <- data |>
    dplyr::right_join(species_list)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$clean_name) {
      message("Species '", species, "' not found.")
      return(NULL)
    }
    data <- data |> dplyr::filter(clean_name == species)
  }

  # Calculate edges and plot
  plots <- data |>
    dplyr::group_by(clean_name, year, season) |>
    dplyr::summarise(
      `95%` = Hmisc::wtd.quantile(x = lat, weights = total_biomass_kg, probs = 0.95, na.rm = T),
      `50%` = Hmisc::wtd.quantile(x = lat, weights = total_biomass_kg, probs = 0.50, na.rm = T),
      `5%`  = Hmisc::wtd.quantile(x = lat, weights = total_biomass_kg, probs = 0.05, na.rm = T)
    ) |>
    tidyr::pivot_longer(cols = c(`95%`,`50%`,`5%`), names_to = "percentile", values_to = "lat") |>
    dplyr::group_by(clean_name, percentile, season) |>
    dplyr::mutate(rmean  = zoo::rollapplyr(lat, width = 5, FUN = mean, align = "center", partial = T),
           percentile = factor(percentile, levels = c('5%','50%','95%'))) |>
    dplyr::group_by(clean_name) |>
    tidyr::nest() |>
    dplyr::mutate(
      out = purrr::map(data, function(x){
        ggplot2::ggplot() +
          ggplot2::geom_line(data = x,
                             ggplot2::aes(x = year, y = rmean, color = percentile), linetype = 2
          ) +
          ggplot2::geom_smooth(data = x,
                               ggplot2::aes(x = year, y = rmean, color = percentile), method = "lm", se = FALSE
          ) +
          ggplot2::labs(
                        title = "Latitude percentiles",
                          subtitle = "5-year rolling mean",
                          x     = "Year",
                          y     = "Latitude") +
          ggplot2::facet_wrap(~season, nrow = 1) +
          ggplot2::guides(color =
                            ggplot2::guide_legend("Percentiles")) +
          gmRi::scale_color_gmri() +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 12),
            plot.title        = ggplot2::element_text(face = "bold"),
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

## Distance between centroids
#' @title Plot NEFSC Bottom Trawl Center of Biomass
#'
#' @description Function to calculate the annual centers of biomass by season of Mid-Atlantic species and plot latitude and the distance between them.
#'
#' @param species Default is "all", includes Mid-Atlantic species represented in `species.shift::species_list()`
#' @param data Default is "nefsc". `pull_nefsc()` must be run prior in order to run this function.
#' @return Plot of fall and spring centers of latitude. Selecting `all` species will return a list.
#' @export
#' @examples # plot_nefsc_centers(species = "black sea bass", data = nefsc)

plot_nefsc_centers <- function(species = "all", data = "nefsc"){

  # Get species list
  species_list <- species.shift::species_list(source = "nefsc")

  # Base filter
  data <- data |>
    dplyr::right_join(species_list)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$clean_name) {
      message("Species '", species, "' not found.")
      return(NULL)
    }
    data <- data |> dplyr::filter(clean_name == species)
  }

  # Calculate center of biomass and plot
  plots <- data |>
    dplyr::group_by(clean_name, year, season) |>
    dplyr::summarise(
      lat = matrixStats::weightedMean(x = lat, w = total_biomass_kg),
      lon = matrixStats::weightedMean(x = lon, w = total_biomass_kg)
    ) |>
    dplyr::group_by(clean_name) |>
    tidyr::nest() |>
    dplyr::mutate(
      out = purrr::map(data, function(x){
        ggplot2::ggplot() +
          ggplot2::geom_line(data = x,
                             ggplot2::aes(x = year, y = lat, group = year), color = "#535353", alpha = 0.8) +
          ggplot2::geom_point(data = x,
                              ggplot2::aes(x = year, y = lat, color = season)) +
          gmRi::scale_color_gmri() +
          ggplot2::labs(
            title = "Spring-Fall center of latitude",
            y     = "Latitude",
            x     = "Year"
          ) +
          ggplot2::guides(
            color = ggplot2::guide_legend(title = "Season")
          ) +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 12),
            plot.title        = ggplot2::element_text(face = "bold"),
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

## Latitude accumulation (percentile) maps
#' @title Map NEFSC biomass data
#'
#' @description Function to maps distributions of biomass in NEFSC Spring-Fall Bottom Trawl Survey. Color indicates `leading` and `trailing` 10% of kept catch, with `center`representing 80% of surveyed biomass. Yellow contours represent density.
#'
#' @param species Default is "all", includes Mid-Atlantic species represented in `species.shift::species_list()`
#' @param data Default is "nefsc" `nefsc` must be run and named "observer" in order to run this function.
#' @return Map of distribution of biomass along the Northeast US. Selecting `all` species will return a list.
#' @export
#' @examples # map_nefsc(species = "summer flounder", data = "nefsc")
#'
map_nefsc <- function(species = "all", data = "nefsc"){

  # Get species list
  species_list <- species.shift::species_list(source = "nefsc")

  # Base filter
  data <- data |>
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

  # Percentiles by decade
  tmp <- data |>
    dplyr::mutate(decade = 10*year%/%10) |>
    dplyr::group_by(clean_name, decade) |>
    dplyr::summarise(`5%`  = Hmisc::wtd.quantile(lat,  w = total_biomass_kg, probs = 0.05),
                     `95%` = Hmisc::wtd.quantile(lat,  w = total_biomass_kg, probs = 0.95))

  # Join together and plot
  plots <- data |>
    dplyr::mutate(decade = 10*year%/%10) |>
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
                              ggplot2::aes(x = lon, y = lat, color = partition, group = partition, alpha = total_biomass_kg)
          ) +
          ggplot2::stat_density2d(data = x,
                                  ggplot2::aes(x = lon, y = lat), alpha = 0.8, color = "#ebcb27") +
          ggplot2::scale_color_manual(values = c("#363b45", "#00608a","#C1DEFF")) +
          ggplot2::labs(
            title = "Biomass distribrution",
            subtitle = "Center represents 80% of surveyed biomass"
          ) +
          ggplot2::facet_wrap(
            ~stringr::str_to_title(decade),
            nrow = 1
          ) +
          ggplot2::guides(
            color = ggplot2::guide_legend(title = "Percentiles"),
            alpha = ggplot2::guide_legend(title = "Biomass (kg)")
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
