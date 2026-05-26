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
    "New England",
    "Mid-Atlantic",
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
           region = str_replace(region, "North Atlantic", "New England"),
           region = str_replace(region, "Mid Atlantic", "Mid-Atlantic"),
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

## Trend plots
#' @title MRIP Catch Estimate Trends
#'
#' @description Function to calculate trends and summarise into an aesthetic table.
#'
#' @param species Mid-Atlantic managed species as listed in `species_list(source = "mrip")`
#' @param data Catch estimates from `pull_mrip_catch()`
#' @return gtable of recreational harvest trends
#' @import
#' @export
#' @example # plot_catch_trends(species = "summer flounder", data = catch)
#'
plot_catch_trends <- function(species = "all", data = "NULL") {

  # Get species list
  species_list <- species.shifts::species_list(source = "mrip")

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
    "New England",
    "Mid-Atlantic",
    "South Atlantic",
    "Gulf of Mexico"
  )

  # Join to validated species list
  data <- data |>
    dplyr::right_join(species_list)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$comname) {
      message("Species '", species, "' not found in landings data.")
      return(NULL)
    }
    catch_data <- data |> dplyr::filter(comname == species)
  }

  # Build per-species catch data
  species_catch <- catch_data |>
    dplyr::group_by(comname) |>
    tidyr::expand(year, state) |>
    dplyr::left_join(catch_data , by = c("year", "state", "comname")) |>
    dplyr::mutate(
      harvest_a_b1_numbers = dplyr::if_else(
        is.na(harvest_a_b1_numbers), 0, harvest_a_b1_numbers),
      total_catch_a_b1_b2_numbers = dplyr::if_else(
        is.na(total_catch_a_b1_b2_numbers), 0, total_catch_a_b1_b2_numbers),
      region = dplyr::case_when(
        state %in% c("Maine", "New Hampshire", "Massachusetts",
                     "Rhode Island", "Connecticut")           ~ "New England",
        state %in% c("New York", "New Jersey", "Delaware",
                     "Maryland", "Virginia")                  ~ "Mid-Atlantic",
        state %in% c("North Carolina", "South Carolina",
                     "Georgia", "Florida")                    ~ "South Atlantic",
        state %in% c("Alabama", "Mississippi",
                     "Louisiana", "Texas")                    ~ "Gulf of Mexico"),
      region = factor(region, levels = region_levels)
    )|>
    dplyr::filter(!region == "Gulf of Mexico")

  # Compute trends per species
  trends <- species_catch %>% # x
    group_by(year) %>%
    summarise(
      total_catch_annual = sum(harvest_a_b1_numbers, na.rm = T), .groups = "drop") %>%
    left_join(species_catch) %>% # x
    mutate(
      region_state = str_c(region, state, sep = "_"),
      harvest_frac = round(harvest_a_b1_numbers/total_catch_annual, 2)*100,
      harvest_frac = if_else(is.na(harvest_frac), 0, harvest_frac),
      presence     = if_else(total_catch_a_b1_b2_numbers > 0, 1, 0)) %>%
    split(.$region_state) %>%
    map_dfr(function(x){
      # Make sure we're going in order
      x_ts <- arrange(x, year) %>%
        tail(15)

      # If there is not enough data, say it
      # if(nrow(x_ts) < 5){
      if(x_ts %>%
         filter(x_ts$harvest_a_b1_numbers > 0) %>%
         nrow() < 5){
        return(
          tibble(
            "presence" = list(x_ts$presence),
            "total_catch" = list(x_ts$harvest_a_b1_numbers), # Shows a flat line
            #"total_catch" = NA, # Shows sparkline as blank

            #"catch_trend" = NA_character_, # Reports text as Blank
            "catch_trend" = "Limited Data", # Reports data limitation

            "harvest_fraction" = list(x_ts$harvest_frac),
            # "harvest_fraction" = NA,

            #"fract_trend" = NA_character_,
            "fract_trend" = "Limited Data" # Reports data limitation
          )
        )}

      # If theres sufficient data, test it
      # Trends in state catch
      catch_trend    <- trend::mk.test(x_ts$harvest_a_b1_numbers)$p.value < 0.05
      catch_rate     <- coef(lm(harvest_a_b1_numbers ~ year, data = x_ts))[[2]]
      catch_rate_msg <- if_else(catch_rate > 0, "Increasing", "Decreasing")
      catch_msg <- ifelse(
        catch_trend,
        catch_rate_msg,
        "Stable")

      # Trends in the fraction of total catch
      fract_trend    <- trend::mk.test(x_ts$harvest_frac)$p.value < 0.05
      fract_rate     <- coef(lm(harvest_frac ~ year, data = x_ts))[[2]]
      fract_rate_msg <- if_else(fract_rate > 0, "Increasing", "Decreasing")
      fract_msg <- ifelse(
        fract_trend,
        fract_rate_msg,
        "Stable")

      # Return the trend result and a list column of the data
      return(
        tibble(
          "presence" = list(x_ts$presence),
          "total_catch" = list(x_ts$harvest_a_b1_numbers),
          "catch_trend" = catch_msg,
          "harvest_fraction" = list(x_ts$harvest_frac),
          "fract_trend" = fract_msg))


    }, .id = "var_area")  %>%

    # Split the var area column back
    separate(var_area, into = c("region", "state"), sep = "_") %>%
    mutate(
      state = factor(state, levels = states_ns),
      region = factor(region, levels = region_levels)) %>%
    # Labels for catch trends
    dplyr::mutate(
      catch_dir =  case_match(
        catch_trend,
        "Stable" ~ "minus",
        "Increasing" ~ "arrow-up",
        "Decreasing" ~ "arrow-down",
        "Limited Data" ~ NA_character_),
      .before = "harvest_fraction") |>
    # Labels for harvest trends
    dplyr::mutate(
      fract_dir =  case_match(
        fract_trend,
        "Stable" ~ "minus",
        "Increasing" ~ "arrow-up",
        "Decreasing" ~ "arrow-down",
        "Limited Data" ~ NA_character_))  %>%
    arrange(region, state)

  # Build table (gt)
  out <- trends |>
    dplyr::arrange(region, state) |>
    gt::gt(groupname_col = "region") |>
    gt::tab_header(title = stringr::str_to_sentence(species),
                   subtitle =  "MRIP Directed Trip Trends of the Last 15 Years; PSE <= 30%") |>
    gt::tab_options(row_group.as_column = T)  |>
    # Add a Sparkline for catch
    gtExtras::gt_plt_sparkline(
      column = total_catch,
      type = "shaded",
      palette = c(
        "black",
        rep("transparent", 3),
        "#00608a"),
      same_limit = F) |>
    # Presence/Absence Indication
    gtExtras::gt_plt_winloss(
      presence, type = "pill",
      palette = c("#057872", "lightgray", "lightgray")) |>
    # Sparkline for the fraction of annual catch
    gtExtras::gt_plt_sparkline(
      column = harvest_fraction,
      type = "shaded",
      palette = c(
        "black",
        "black",
        rep("transparent", 2),
        "#ea4f12"),
      same_limit = T) |>
    # Up/down arrows
    gt::fmt_icon(
      columns = dplyr::ends_with("dir"),
      fill_color = c(
        "minus" = "lightgray",
        "arrow-up" = "#057872",
        "arrow-down" = "#ea4f12"
      ))  |>
    # Spanner for the takeaway sections
    gt::tab_spanner(
      label = "Total Catch Trends:",
      columns = c(total_catch, catch_trend, catch_dir)) |>
    # Spanner for the harvest fraction trends
    gt::tab_spanner(
      label = "Catch Proportion Trends:",
      columns = c(harvest_fraction, fract_trend, fract_dir)) |>
    gt::fmt_missing(
      columns = gt::everything(),
      missing_text = "") |>
    gt::cols_label(
      presence = "Presence/Absence",
      state = "State",
      total_catch = "Total Recreational Harvest",
      catch_trend = "Catch Trend",
      catch_dir = "",
      harvest_fraction = "% of Recreational Harvest",
      fract_trend = "Proportion Trend",
      fract_dir = "",) |>
    gt::cols_align(
      align = "left",
      columns = "state") |>
    gt::opt_vertical_padding(scale = 0.5) |>
    gt::opt_table_font(font = list(gt::google_font(name = "Avenir")))

  return(out)
}

