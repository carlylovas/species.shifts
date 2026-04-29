### GARFO Federal Permits Data ###
# box_path  <- "/Users/clovas/Library/CloudStorage/Box-Box/MAFMC-25 Data/"
# proj_path <- paste0(box_path, "Non-confidential/Permits/")

## Data pull ----

#' @title Pull federal permits data
#'
#' @description Function to pull, clean and geocode GARFO commercial fishing permits from pre-existing confidential repository. Note that due to spelling error, geocoding principal ports removes 1% of permit entries, and takes approximately 25 minutes to run.
#'
#' @param proj_path Local path to data file
#' @return Data frame of permits; includes year, prinicpal port and state, permit type, target species, and category (commerical, for-hire).
#' @export
#' @examples # permits <- pull_permits(proj_path = proj_path)
pull_permits <- function(proj_path){

  state_names <- c(
    ME = "Maine",          NH = "New Hampshire", MA = "Massachusetts",
    CT = "Connecticut",    RI = "Rhode Island",  NY = "New York",
    NJ = "New Jersey",     PA = "Pennsylvania",  MD = "Maryland",
    DE = "Delaware",       VA = "Virginia",      NC = "North Carolina",
    SC = "South Carolina", GA = "Georgia",       FL = "Florida"
  )

  council_map <- c(
    "Maine" = "New England",         "New Hampshire" = "New England",
    "Massachusetts" = "New England", "Connecticut" = "New England",
    "Rhode Island" = "New England",  "New York" = "Mid-Atlantic",
    "New Jersey" = "Mid-Atlantic",      "Pennsylvania" = "Mid-Atlantic",
    "Maryland" = "Mid-Atlantic",        "Delaware" = "Mid-Atlantic",
    "Virginia" = "Mid-Atlantic",        "North Carolina" = "South Atlantic",
    "South Carolina" = "South Atlantic","Georgia" = "South Atlantic",
    "Florida" = "South Atlantic"
  )

  # Read in multiple excel files
  read_files <- function(file_name){
  out <- readxl::read_excel(paste0(file_name))
  return(out)
  }

  data <- tibble::tibble("file_path" = list.files(proj_path, pattern = ".xlsx", full.names = TRUE)) |>
    dplyr::mutate("data" = purrr::map(file_path, read_files)) |>
    tidyr::unnest(data) |>
    dplyr::select(!file_path) |>
    janitor::clean_names()

  # Clean data
  dat_clean <- data |>
    dplyr::select(ap_num, vp_num, ap_year, pport, ppst, black_sea_bass:tilefish) |>
    tidyr::pivot_longer(
      cols      = black_sea_bass:tilefish,
      names_to  = "target_species",
      values_to = "permit_category",
      values_drop_na = TRUE
    ) |>
    tidyr::separate(permit_category,
                    c("a","b", "c", "d", "e", "f", "g", "h", "i", "j"),
                    sep = ",") |>
    tidyr::pivot_longer(
      cols = a:j,
      names_to = "cols",
      values_to = "category",
      values_drop_na = TRUE
    ) |>
    dplyr::select(!cols) |>
    dplyr::mutate(
      count = 1,
      permit = paste(target_species, category, sep = "_"),
      row = dplyr::row_number()
    ) |>
    dplyr::select(!c(target_species, category)) |>
    dplyr::arrange(permit) |>
    tidyr::pivot_wider(names_from = permit, values_from = count, names_expand = TRUE, values_fill = list(count = 0)) |>
    dplyr::select(!row) |>
    dplyr::arrange(ap_year) |>
    dplyr::group_by(ap_num, ap_year, vp_num, pport, ppst) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        sum
      )
    )

  permits <- dat_clean |>
    janitor::clean_names() |> # seems redundant but is necessary
    dplyr::ungroup() |>
    tidyr::pivot_longer(cols = black_sea_bass_1:tilefish_d, names_to = "permit", values_to = "count") |>
    dplyr::distinct() |>
    dplyr::mutate(target = dplyr::case_when(
      stringr::str_starts(permit, "black_sea_bass") ~ "black sea bass",
      stringr::str_starts(permit, "bluefish") ~ "bluefish",
      stringr::str_starts(permit, "dogfish") ~ "dogfish",
      stringr::str_starts(permit, "gen_cat") ~ "scallop",
      stringr::str_starts(permit, "herring") ~ "herring",
      stringr::str_starts(permit, "hms_") ~ "squid",
      stringr::str_starts(permit, "lobster") ~ "lobster",
      stringr::str_starts(permit, "monkfish") ~ "monkfish",
      stringr::str_starts(permit, "multispecies") ~ "multispecies", # who is included in the multispecies complex
      stringr::str_starts(permit, "quahog") ~ "ocean quahog", # check
      stringr::str_starts(permit, "red_crab") ~ "red crab",
      stringr::str_starts(permit, "scup") ~ "scup",
      stringr::str_starts(permit, "sea_scallop") ~ "scallop",
      stringr::str_starts(permit, "skate") ~ "skate",
      stringr::str_starts(permit, "squid_mack") ~ "squid/mackerel/butterfish", # will need to split out later
      stringr::str_starts(permit, "summer_flounder") ~ "summer flounder",
      stringr::str_starts(permit, "surfclam") ~ "surf clam",
      stringr::str_starts(permit, "tilefish") ~ "tilefish"# includes golden and blueline
    )) |>
    dplyr::filter(count > 0) |>
    dplyr::select(ap_year, ap_num, vp_num, pport, ppst, permit, target)

  # Adding permit category
  permits <- permits |>
    dplyr::mutate(category = dplyr::case_when(
      permit %in% c("black_sea_bass_1",
                    "bluefish_1",
                    "dogfish_1",
                    "gen_cat_scallop_1","gen_cat_scallop_1a", "gen_cat_scallop_1b","gen_cat_scallop_a","gen_cat_scallop_b","gen_cat_scallop_c",
                    "herring_1","herring_2","herring_3","herring_a","herring_b","herring_c","herring_d","herring_e",
                    "hms_squid_1",
                    "lobster_1","lobster_2","lobster_a1","lobster_a2","lobster_a23","lobster_a3","lobster_a4","lobster_a5","lobster_a5w","lobster_a6","lobster_aoc",
                    "monkfish_a","monkfish_b","monkfish_c","monkfish_d","monkfish_e","monkfish_f","monkfish_h",
                    "multispecies_1","multispecies_2","multispecies_3","multispecies_4","multispecies_5","multispecies_6","multispecies_7","multispecies_8",
                    "multispecies_a","multispecies_b","multispecies_c","multispecies_d","multispecies_e","multispecies_f","multispecies_g","multispecies_h","multispecies_ha","multispecies_hb","multispecies_j","multispecies_k",
                    "quahog_6","quahog_7",
                    "red_crab_a","red_crab_b","red_crab_c",
                    "scup_1",
                    "sea_scallop_2","sea_scallop_3","sea_scallop_4","sea_scallop_5","sea_scallop_6","sea_scallop_7","sea_scallop_8","sea_scallop_9",
                    "skate_1",
                    "squid_mack_butter_1","squid_mack_butter_1a","squid_mack_butter_1b","squid_mack_butter_1c","squid_mack_butter_3","squid_mack_butter_4","squid_mack_butter_5","squid_mack_butter_6","squid_mack_butter_t1","squid_mack_butter_t2", "squid_mack_butter_t3",
                    "summer_flounder_1",
                    "surfclam_1",
                    "tilefish_1","tilefish_a","tilefish_b","tilefish_c","tilefish_d") ~ "commercial",
      permit %in% c("black_sea_bass_2", "bluefish_2","multispecies_i",
                    "scup_2","squid_mack_butter_2","summer_flounder_2","tilefish_2")  ~ "charter/for-hire",
      permit == "tilefish_3" ~ "recreational"))

  # Parsing out multiple species permits

  ### squid/mackerel/butterfish
  smb <- permits |>
    dplyr::filter(target == "squid/mackerel/butterfish") |>
    dplyr::mutate(target = dplyr::case_when(
      permit %in% c("squid_mack_butter_1", "squid_mack_butter_1a","squid_mack_butter_1b","squid_mack_butter_1c") ~ "longfin squid",
      permit %in% c("squid_mack_butter_4", "squid_mack_butter_t1", "squid_mack_butter_t2", "squid_mack_butter_t3") ~ "atlantic mackerel",
      permit %in% c("squid_mack_butter_2", "squid_mack_butter_3") ~ "squid/mackerel/butterfish", # 2-charter party, 3-squid/butterfish incidental catch
      permit == "squid_mack_butter_5" ~ "northern shortfin squid", # illex squid
      permit == "squid_mack_butter_6" ~ "butterfish",
    ))

  ### multispecies (groundfish) complex
  # groundfish <- tibble(comname = c("atlantic cod", "haddock", "yellowtail flounder", "pollock", "acadian redfish", "winter flounder",
  #                                  "american plaice", "witch flounder", "windowpane flounder", "white hake", "halibut", "atlantic wolffish",
  #                                  "silver hake", "red hake", "offshore hake", "ocean pout")) |> nest()
  #
  # multispecies <- permits |>
  #   filter(target == "multispecies") |>
  #   select(permit) |>
  #   distinct() |>
  #   arrange(permit) |>
  #   bind_cols(groundfish) |>
  #   unnest(data)

#### Note: Because the multispecies permits differ by gear and mesh type, rather than target species, they will not be joined back to the overall permit data.
#### They can be added back in later if needed to analyze specific species within the complex, otherwise will create duplicated values of permits.

  # Combine ----
  all <- permits |>
    dplyr::filter(!target == "squid/mackerel/butterfish") |>
    dplyr::full_join(smb)

  # Geocode ----
  ports <- all |>
    dplyr::select(pport, ppst) |>
    dplyr::distinct() |>
    tidygeocoder::geocode(city  = pport, state = ppst)

  geocodes <- ports |>
    tidyr::drop_na() |>
    dplyr::arrange(ppst) |>
    dplyr::mutate(port = paste(pport, ppst, sep = ", ")) |>
    dplyr::select(port, lat, long)

  temp <- permits |>
    dplyr::mutate(port = paste(pport,ppst,sep=", ")) |>
    dplyr::left_join(geocodes) |>
    dplyr::filter(is.na(lat)) |>
    dplyr::select(!c(lat,long)) |>
    fuzzyjoin::stringdist_left_join(geocodes) |>
    tidyr::drop_na() |>
    tidyr::separate(port.y, into= c("clean_port", "state"), sep = ", ") |>
    dplyr::mutate(match = ifelse(ppst == state, T,F)) |>
    dplyr::filter(!match == FALSE) |>
    dplyr::select(!c(pport, port.x,state,match)) |>
    dplyr::rename("pport" = "clean_port") |>
    dplyr::relocate(pport, .before = ppst)

  out <- permits |>
    dplyr::mutate(port = paste(pport,ppst,sep=", ")) |>
    dplyr::left_join(geocodes) |>
    dplyr::filter(!is.na(lat)) |>
    dplyr::select(!port) |>
    dplyr::full_join(temp) |>
    dplyr::arrange(ap_year, ap_num) |>
    dplyr::distinct() |>
    dplyr::mutate(
      state_full = dplyr::recode(ppst, !!!state_names),
      council    = factor(council_map[state_full], levels = c("New England", "Mid-Atlantic", "South Atlantic"))
    ) |>
    dplyr::filter(!is.na(council)) # removes non-East coast states (AK, AL, TX, etc.)

  return(out)

}


## Plotting ----
## State proportions
#' @title Plotting permit proportions by state
#'
#' @description Function to plot proportions of landings across states for Mid-Atlantic species.
#'
#' @param data Landings outputs from `pull_permits()`
#' @param species Mid-Atlantic managed species as listed in `species_list(source = "permits")`
#' @return List of faceted plots.
#' @import ggplot2
#' @export
#'
#' @examples # plot_state_permits(species = "summer flounder", data = permits)

plot_state_permits <- function(species = "all", data = "permits") {


  # Get species list
  species_list <- species.shifts::species_list(source = "permits")

  # Base filter
  data <- data |>
    dplyr::right_join(species_list)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$clean_name) {
      message("Species '", species, "' not found in permits data.")
      return(NULL)
    }
    data <- data |> dplyr::filter(clean_name == species)
  }

  # Build plots only for relevant species
  plots <- data |>
    dplyr::group_by(ap_year, permit, clean_name, category, state_full) |>
    dplyr::summarise(count =
                       dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(ap_year, permit) |>
    dplyr::mutate(total_count = sum(count),
           prop  = (count / total_count),
           facet =
             stringr::str_to_title(
               paste(
                 permit, category, sep = " - "
                 )
               )) |>
    dplyr::group_by(clean_name) |>
    tidyr::nest() |>
    tidyr::drop_na() |>
    dplyr::mutate(
      out = purrr::map2(data, clean_name, function(x, y) {
        ggplot2::ggplot(data = x) +
          ggplot2::geom_col(
            ggplot2::aes(x = ap_year, y = prop, fill = state_full)
          ) +
          gmRi::scale_fill_gmri() +
          ggplot2::facet_wrap(
            ~facet,
            nrow = 1
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(nrow = 2)
          ) +
          ggplot2::labs(
            title = "Proportion of permits by state",
            x = "Year",
            y = "Proportion"
          ) +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 13),
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
#' @title Plotting permit proportions by Management Councils.
#'
#' @description Function to plot proportions of permits across states for Mid-Atlantic species.
#'
#' @param data Landings outputs from `pull_permits()`
#' @param species Mid-Atlantic managed species as listed in `species_list(source = "permits")`
#' @return List of faceted plots.
#' @import ggplot2
#' @export
#'
#' @examples # plot_council_permits(species = "summer flounder", data = permits)

plot_council_permits <- function(species = "all", data = "permits") {

  # Get species list
  species_list <- species.shifts::species_list(source = "permits")

  # Base filter
  data <- data |>
    dplyr::rename("comname" = "target") |>
    dplyr::right_join(species_list)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$clean_name) {
      message("Species '", species, "' not found in permits data.")
      return(NULL)
    }
    data <- data |> dplyr::filter(clean_name == species)
  }

  # Build plots only for relevant species
  plots <- data |>
    dplyr::group_by(ap_year, permit, clean_name, category, council) |>
    dplyr::summarise(count =
                       dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(ap_year, permit) |>
    dplyr::mutate(total_count = sum(count),
                  prop  = (count / total_count),
                  facet =
                    stringr::str_to_title(
                      paste(
                        permit, category, sep = " - "
                      )
                    )) |>
    dplyr::group_by(clean_name) |>
    tidyr::nest() |>
    tidyr::drop_na() |>
    dplyr::mutate(
      out = purrr::map2(data, clean_name, function(x, y) {
        ggplot2::ggplot(data = x) +
          ggplot2::geom_col(
            ggplot2::aes(x = ap_year, y = prop, fill = council)
          ) +
          ggplot2::scale_fill_manual(values = c("#363b45", "#00608a","#C1DEFF")) +
          ggplot2::facet_wrap(
            ~facet,
            nrow = 1
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(nrow = 1)
          ) +
          ggplot2::labs(
            title = "Proportion of permits by council",
            x = "Year",
            y = "Proportion"
          ) +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 13),
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

## Permit distribution edges
#' @title Plotting permit 5th, 50th, and 95th weighted percentiles of latitudinal distribtion.
#'
#' @description Function to plot percentiles of permits across states for Mid-Atlantic species.
#'
#' @param data Landings outputs from `pull_permits()`
#' @param species Mid-Atlantic managed species as listed in `species_list(source = "permits")`
#' @return List of faceted plots.
#' @import ggplot2
#' @export
#'
#' @examples # plot_permit_edges(species = "summer flounder", data = permits)
plot_permit_edges <- function(species = "all", data = "permits"){

  # Get species list
  species_list <- species.shifts::species_list(source = "permits")

  # Base filter
  data <- permits |>
    dplyr::rename("comname" = "target") |>
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
    dplyr::mutate(
      dplyr::across(lat:long, \(x) round(x, digits = 1)),
      decade = 10*ap_year%/%10,
      facet  = stringr::str_to_title(
        paste(permit, category, sep = " - ")))|>
    dplyr::group_by(ap_year, clean_name, facet, lat, long) |>
    dplyr::summarise(
      count = dplyr::n()) |>
    dplyr::group_by(ap_year, clean_name, facet) |>
    dplyr::summarise(
      `95%` = Hmisc::wtd.quantile(x = lat, weights = count, probs = 0.95, na.rm = T),
      `50%` = Hmisc::wtd.quantile(x = lat, weights = count, probs = 0.50, na.rm = T),
      `5%`  = Hmisc::wtd.quantile(x = lat, weights = count, probs = 0.05, na.rm = T)
    ) |>
    tidyr::pivot_longer(cols = c(`95%`,`50%`,`5%`), names_to = "percentile", values_to = "lat") |>
    dplyr::group_by(clean_name, percentile, facet) |>
    dplyr::mutate(rmean  = zoo::rollapplyr(lat, width = 5, FUN = mean, align = "center", partial = T),
                  percentile = factor(percentile, levels = c('5%','50%','95%'))) |>
    dplyr::group_by(clean_name) |>
    tidyr::nest() |>
    dplyr::mutate(
      out = purrr::map(data, function(x){
        ggplot2::ggplot() +
          ggplot2::geom_line(data = x,
                             ggplot2::aes(x = ap_year, y = rmean, color = percentile), linetype = 2
          ) +
          ggplot2::geom_smooth(data = x,
                               ggplot2::aes(x = ap_year, y = rmean, color = percentile), method = "lm", se = FALSE
          ) +
          ggplot2::labs(
            title = "Latitude percentiles",
            subtitle = "5-year rolling mean",
            x     = "Year",
            y     = "Latitude") +
          ggplot2::facet_wrap(~facet, nrow = 1) +
          ggplot2::guides(color =
                            ggplot2::guide_legend("Percentiles")) +
          gmRi::scale_color_gmri() +
          ggplot2::theme(
            text              = ggplot2::element_text(family = "Avenir", size = 13),
            plot.title        = ggplot2::element_text(face = "bold"),
            legend.position   = "bottom",
            legend.box        = "vertical",
            strip.background  = ggplot2::element_blank(),
            strip.text        = ggplot2::element_text(hjust = 0, face = "bold", size = 15),
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



## Map permits
#' @title Map permits by type along NEUS coast.
#'
#' @description Function to plot distributions of permits along the Northeast US coastline.
#' @param species Default is "all", includes Mid-Atlantic species represented in `species.shifts::species_list()`
#' @param data Default is "permits" `pull_permits` must be run and named "permits" in order to run this function.
#' @return Map of distribution of observed kept catch along the Northeast US. Selecting `all` species will return a list.
#' @export
#' @examples # map_permits(species = "summer flounder", data = "permits")
#'
map_permits <- function(species = "all", data = "permits"){

  # Get species list
  species_list <- species.shifts::species_list(source = "permits")

  # Base filter
  data <- permits |>
    dplyr::rename("comname" = "target") |>
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

  # plot
  plots <- data |>
    dplyr::mutate(
      dplyr::across(lat:long, \(x) round(x, digits = 1)),
      decade = 10*ap_year%/%10,
      facet  = stringr::str_to_title(
        paste(permit, category, sep = " - "))) |>
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
                              ggplot2::aes(x = long, y = lat, color = facet)
          ) +
          gmRi::scale_color_gmri() +
          ggplot2::facet_grid(
            ggplot2::vars(permit),
            ggplot2::vars(decade)
            ) +
          ggplot2::guides(
            color = ggplot2::guide_legend(title = "Permit types")
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

