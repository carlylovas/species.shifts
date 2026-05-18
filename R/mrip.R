## Catch estimates and plotting ##
### Pulls ACCSP Catch Estimate Data from a central repo ###
### Access public non-confidential data here: https://safis.accsp.org/accsp_prod/f?p=1490:2211:17151574143887 ###

# box_path  <- "/Users/clovas/Library/CloudStorage/Box-Box/MAFMC-25 Data/"
# proj_path <- paste0(box_path, "Non-confidential/ACCSP/Catch_Estimates.csv")

## Data pull ----

#' @title Pull federal MRIP catch estimates
#'
#' @description Function to pull and clean recreational catch estimates. Data can be acquired from the ACCSP Data Warehouse. Please download as 'Catch_Estimates.csv'
#'
#' @param proj_path Local path to data file
#' @return Data frame of catch estimates. Please refer to [MRIP Data Dictionary](https://www.fisheries.noaa.gov/s3//2025-03/MRIP-Data-User-Handbook_March_2025_update.pdf.pdf) for more information regarding estimates.
#' @export
#' @examples # catch <- pull_mrip_catch(proj_path = proj_path)
#'

pull_mrip_catch <- function(proj_path){

  # States, listed north to south
  states_ns <- c(
    "Maine",
    "New Hampshire",
    "Massachusetts",
    "Rhode Island",
    "Connecticut",
    "New York",
    "New Jersey",
    "Pennsylvania",
    "Delaware",
    "Maryland",
    "Virginia",
    "North Carolina",
    "South Carolina",
    "Georgia",
    "Florida",
    "Alabama",
    "Mississippi",
    "Louisiana"
  )

  region_levels <- c(
    "North Atlantic",
    "Mid Atlantic",
    "South Atlantic",
    "Gulf of Mexico"
  )

  ## Catch estimates ----
  data <- readr::read_csv(proj_path) |>
    janitor::clean_names() |>
    dplyr::filter(year %in% seq(2010,2024)) |>
    dplyr::filter(pse_harvest_a_b1_numbers <= 30) |>
    dplyr::mutate(common_name = tolower(common_name),
           state  = stringr::str_to_title(state),
           state  = factor(state, levels = states_ns),
           region = factor(region, levels = region_levels))

  data <- data |>
    dplyr::mutate(comname = sub("^(.*?),\\s*(.*)$", "\\2 \\1", data$common_name)) |>
    dplyr::mutate(
      comname = stringr::str_replace(comname, "shark, dogfish", "dogfish"),
      comname = stringr::str_replace(comname, "golden tilefish", "tilefish"),
      comname = stringr::str_replace(comname, "goosefish", "monkfish"),
    )
  return(data)
}

#' ## Trend plots
#' #' @title MRIP Catch Estimate Trends
#' #'
#' #' @description Function to calculate trends and summarise into an aesthetic table.
#' #'
#' #' @param species Mid-Atlantic managed species as listed in `species_list(source = "permits")`
#' #' @param data Landings outputs from `pull_mrip_catch()`
#' #' @return gtable of recreational harvest trends
#' #' @import
#' #' @export
#' #' @example # not run
#' #'
#' plot_catch_trends <- function(species = "all", catch_data) {
#'
#'   # Get species list
#'   species_list <- species.shifts::species_list(source = "mrip")
#'
#'   # Join to validated species list
#'   catch_data <- catch_data |>
#'     dplyr::right_join(species_list)
#'
#'   # Determine which species to process
#'   target_species <- if (species == "all") {
#'     unique(catch_data$clean_name)
#'   } else {
#'     if (!species %in% catch_data$clean_name) {
#'       message("Species '", species, "' not found in catch data.")
#'       return(NULL)
#'     }
#'     species
#'   }
#'
#'   region_levels <- c("North Atlantic", "Mid Atlantic", "South Atlantic", "Gulf of Mexico")
#'
#'   states_ns <- c(
#'     "Maine", "New Hampshire", "Massachusetts", "Rhode Island", "Connecticut",
#'     "New York", "New Jersey", "Delaware", "Maryland", "Virginia",
#'     "North Carolina", "South Carolina", "Georgia", "Florida",
#'     "Alabama", "Mississippi", "Louisiana", "Texas"
#'   )
#'
#'   # Build per-species catch data
#'   shortlist_catch <- purrr::map(
#'     setNames(target_species, target_species),
#'     function(x) {
#'       species_x <- catch_data |>
#'         dplyr::filter(tolower(clean_name) == tolower(x))
#'
#'       species_x |>
#'         tidyr::expand(year, state) |>
#'         dplyr::left_join(species_x, by = c("year", "state")) |>
#'         dplyr::mutate(
#'           harvest_a_b1_numbers = dplyr::if_else(
#'             is.na(harvest_a_b1_numbers), 0, harvest_a_b1_numbers),
#'           total_catch_a_b1_b2_numbers = dplyr::if_else(
#'             is.na(total_catch_a_b1_b2_numbers), 0, total_catch_a_b1_b2_numbers),
#'           region = dplyr::case_when(
#'             state %in% c("Maine", "New Hampshire", "Massachusetts",
#'                          "Rhode Island", "Connecticut")           ~ "North Atlantic",
#'             state %in% c("New York", "New Jersey", "Delaware",
#'                          "Maryland", "Virginia")                  ~ "Mid Atlantic",
#'             state %in% c("North Carolina", "South Carolina",
#'                          "Georgia", "Florida")                    ~ "South Atlantic",
#'             state %in% c("Alabama", "Mississippi",
#'                          "Louisiana", "Texas")                    ~ "Gulf of Mexico"),
#'           region = factor(region, levels = region_levels)
#'         )|>
#'         dplyr::filter(!region == "Gulf of Mexico")
#'     }
#'   )
#'
#'   # Compute trends per species
#'   shortlist_trends <- purrr::map(shortlist_catch, function(x) {
#'
#'     prepped <- x |>
#'       dplyr::group_by(year) |>
#'       dplyr::summarise(
#'         total_catch_annual = sum(harvest_a_b1_numbers, na.rm = TRUE),
#'         .groups = "drop") |>
#'       dplyr::left_join(x, by = "year") |>
#'       dplyr::mutate(
#'         region_state = stringr::str_c(region, state, sep = "_"),
#'         harvest_frac = round(harvest_a_b1_numbers / total_catch_annual, 2) * 100,
#'         harvest_frac = dplyr::if_else(is.na(harvest_frac), 0, harvest_frac),
#'         presence     = dplyr::if_else(total_catch_a_b1_b2_numbers > 0, 1, 0)
#'       )
#'
#'     # Split into per-region-state list using a temp variable to avoid pipe placeholder issue
#'     state_list <- base::split(prepped, prepped$region_state)
#'
#'     purrr::map_dfr(state_list, function(state_df) {
#'
#'       x_ts <- state_df |>
#'         dplyr::arrange(year) |>
#'         tail(15)
#'
#'       # Return limited-data row if fewer than 5 non-zero harvest years
#'       if (nrow(dplyr::filter(x_ts, harvest_a_b1_numbers > 0)) < 5) {
#'         return(tibble::tibble(
#'           presence         = list(x_ts$presence),
#'           total_catch      = list(x_ts$harvest_a_b1_numbers),
#'           catch_trend      = "Limited Data",
#'           harvest_fraction = list(x_ts$harvest_frac),
#'           fract_trend      = "Limited Data"
#'         ))
#'       }
#'
#'       # Trend in state catch
#'       catch_trend <- trend::mk.test(x_ts$harvest_a_b1_numbers)$p.value < 0.05
#'       catch_rate  <- stats::coef(stats::lm(harvest_a_b1_numbers ~ year, data = x_ts))[[2]]
#'       catch_msg   <- if (catch_trend) {
#'         ifelse(catch_rate > 0, "Increasing", "Decreasing")
#'       } else "Stable"
#'
#'       # Trend in harvest fraction
#'       fract_trend <- trend::mk.test(x_ts$harvest_frac)$p.value < 0.05
#'       fract_rate  <- stats::coef(stats::lm(harvest_frac ~ year, data = x_ts))[[2]]
#'       fract_msg   <- if (fract_trend) {
#'         ifelse(fract_rate > 0, "Increasing", "Decreasing")
#'       } else "Stable"
#'
#'       tibble::tibble(
#'         presence         = list(x_ts$presence),
#'         total_catch      = list(x_ts$harvest_a_b1_numbers),
#'         catch_trend      = catch_msg,
#'         harvest_fraction = list(x_ts$harvest_frac),
#'         fract_trend      = fract_msg
#'       )
#'
#'     }, .id = "var_area") |>
#'       tidyr::separate(var_area, into = c("region", "state"), sep = "_") |>
#'       dplyr::mutate(
#'         state  = factor(state,  levels = states_ns),
#'         region = factor(region, levels = region_levels),
#'         catch_dir = dplyr::recode_values(
#'           catch_trend,
#'           "Stable"       ~ "minus",
#'           "Increasing"   ~ "arrow-up",
#'           "Decreasing"   ~ "arrow-down",
#'           "Limited Data" ~ NA_character_),
#'         fract_dir = dplyr::recode_values(
#'           fract_trend,
#'           "Stable"       ~ "minus",
#'           "Increasing"   ~ "arrow-up",
#'           "Decreasing"   ~ "arrow-down",
#'           "Limited Data" ~ NA_character_)
#'       ) |>
#'       dplyr::arrange(region, state)
#'   })
#'
#'    #return(shortlist_trends)
#'
#'   out <- shortlist_trends |>
#'     purrr::imap(function(trends_x, species_x){
#'
#'       # Make the table on proportions only
#'       trends_table <- trends_x |>
#'         dplyr::arrange(region, state) |>
#'         gt::gt(groupname_col = "region") |>
#'         gt::tab_header(
#'           title = Hmisc::html(
#'             stringr::str_c(
#'               "Species of Interest: ",
#'               stringr::str_to_title(species_x)
#'               )),
#'           subtitle =  "MRIP Recreational Catch Trends of the Last 15 Years")  |>
#'         gt::tab_options(row_group.as_column = T)  |>
#'         # Add a Sparkline for catch
#'         gtExtras::gt_plt_sparkline(
#'           column = total_catch,
#'           type = "shaded",
#'           palette = c(
#'             "black",
#'             rep("transparent", 3),
#'             "#00608a"),
#'           same_limit = F) |>
#'         # Presence/Absence Indication
#'         gtExtras::gt_plt_winloss(
#'           presence, type = "pill",
#'           palette = c("#057872", "lightgray", "lightgray")) |>
#'         # Sparkline for the fraction of annual catch
#'         gtExtras::gt_plt_sparkline(
#'           column = harvest_fraction,
#'           type = "shaded",
#'           palette = c(
#'             "black",
#'             "black",
#'             rep("transparent", 2),
#'             "#ea4f12"),
#'           same_limit = T) |>
#'         # Up/down arrows
#'         gt::fmt_icon(
#'           columns = dplyr::ends_with("dir"),
#'           fill_color = c(
#'             "minus" = "lightgray",
#'             # "arrow-up" = "seagreen",
#'             # "arrow-down" = "tomato"
#'             "arrow-up" = "#057872",
#'             "arrow-down" = "#ea4f12"
#'           ))  |>
#'         # Spanner for the takeaway sections
#'         gt::tab_spanner(
#'           label = "Total Catch Trends:",
#'           columns = c(total_catch, catch_trend, catch_dir)) |>
#'         # Spanner for the harvest fraction trends
#'         gt::tab_spanner(
#'           label = "Catch Proportion Trends:",
#'           columns = c(harvest_fraction, fract_trend, fract_dir)) |>
#'         gt::fmt_missing(
#'           columns = gt::everything(),
#'           missing_text = "") |>
#'         gt::cols_label(
#'           presence = "Presence/Absence",
#'           state = "State",
#'           total_catch = "Total Recreational Harvest",
#'           catch_trend = "Catch Trend",
#'           catch_dir = "",
#'           harvest_fraction = "% of Recreational Harvest",
#'           fract_trend = "Proportion Trend",
#'           fract_dir = "",) |>
#'         gt::cols_align(
#'           align = "left",
#'           columns = "state") |>
#'         gt::opt_vertical_padding(scale = 0.5) |>
#'         # # enforce the order groups
#'         # row_group_order(
#'         #   groups = region_levels[1:3]) |>
#'         gt::opt_table_font(font = list(gt::google_font(name = "Avenir")))
#'
#'       return(trends_table)
#'
#'     })
#'   if (species == "all") {
#'     return(out |> dplyr::select(species_x, trends_table))
#'   } else {
#'     return(out)
#'   }
#' }
#' test <- plot_catch_trends(species = "summer flounder", catch_data = catch)
#'
