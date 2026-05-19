plot_catch_trends <- function(species = "all", catch_data) {

  # Get species list
  species_list <- species.shifts::species_list(source = "mrip")

  # Join to validated species list
  catch_data <- catch_data |>
    dplyr::right_join(species_list)

  # Validate and filter early if specific species requested
  if (species != "all") {
    if (!species %in% data$comname) {
      message("Species '", species, "' not found in landings data.")
      return(NULL)
    }
    catch_data <- catch_data |> dplyr::filter(comname == species)
  }

  region_levels <- c("North Atlantic", "Mid Atlantic", "South Atlantic", "Gulf of Mexico")

  states_ns <- c(
    "Maine", "New Hampshire", "Massachusetts", "Rhode Island", "Connecticut",
    "New York", "New Jersey", "Delaware", "Maryland", "Virginia",
    "North Carolina", "South Carolina", "Georgia", "Florida",
    "Alabama", "Mississippi", "Louisiana", "Texas"
  )

  # Build per-species catch data
  species_catch <- catch_data |>
    dplyr::group_by(comname) |>
    tidyr::expand(year, state) |>
    dplyr::left_join(catch_data, by = c("year", "state")) |>
    dplyr::mutate(
      harvest_a_b1_numbers = dplyr::if_else(
        is.na(harvest_a_b1_numbers), 0, harvest_a_b1_numbers),
      total_catch_a_b1_b2_numbers = dplyr::if_else(
        is.na(total_catch_a_b1_b2_numbers), 0, total_catch_a_b1_b2_numbers),
      region = dplyr::case_when(
        state %in% c("Maine", "New Hampshire", "Massachusetts",
                     "Rhode Island", "Connecticut")           ~ "North Atlantic",
        state %in% c("New York", "New Jersey", "Delaware",
                     "Maryland", "Virginia")                  ~ "Mid Atlantic",
        state %in% c("North Carolina", "South Carolina",
                     "Georgia", "Florida")                    ~ "South Atlantic",
        state %in% c("Alabama", "Mississippi",
                     "Louisiana", "Texas")                    ~ "Gulf of Mexico"),
          region = factor(region, levels = region_levels)
        )|>
        dplyr::filter(!region == "Gulf of Mexico")

  # Compute trends per species
  # shortlist_trends <- purrr::map(shortlist_catch, function(x) {

    prepped <- species_catch |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        total_catch_annual = sum(harvest_a_b1_numbers, na.rm = TRUE),
        .groups = "drop") |>
      dplyr::left_join(species_catch, by = "year") |>
      dplyr::mutate(
        region_state = stringr::str_c(region, state, sep = "_"),
        harvest_frac = round(harvest_a_b1_numbers / total_catch_annual, 2) * 100,
        harvest_frac = dplyr::if_else(is.na(harvest_frac), 0, harvest_frac),
        presence     = dplyr::if_else(total_catch_a_b1_b2_numbers > 0, 1, 0)
      )

    # Split into per-region-state list using a temp variable to avoid pipe placeholder issue
    state_list <- base::split(prepped, prepped$region_state)

    purrr::map_dfr(state_list, function(state_df) {

      x_ts <- state_list |>
        dplyr::arrange(year) |>
        tail(15)

      # Return limited-data row if fewer than 5 non-zero harvest years
      if (nrow(dplyr::filter(x_ts, harvest_a_b1_numbers > 0)) < 5) {
        return(tibble::tibble(
          presence         = list(x_ts$presence),
          total_catch      = list(x_ts$harvest_a_b1_numbers),
          catch_trend      = "Limited Data",
          harvest_fraction = list(x_ts$harvest_frac),
          fract_trend      = "Limited Data"
        ))
      }

      # Trend in state catch
      catch_trend <- trend::mk.test(x_ts$harvest_a_b1_numbers)$p.value < 0.05
      catch_rate  <- stats::coef(stats::lm(harvest_a_b1_numbers ~ year, data = x_ts))[[2]]
      catch_msg   <- if (catch_trend) {
        ifelse(catch_rate > 0, "Increasing", "Decreasing")
      } else "Stable"

      # Trend in harvest fraction
      fract_trend <- trend::mk.test(x_ts$harvest_frac)$p.value < 0.05
      fract_rate  <- stats::coef(stats::lm(harvest_frac ~ year, data = x_ts))[[2]]
      fract_msg   <- if (fract_trend) {
        ifelse(fract_rate > 0, "Increasing", "Decreasing")
      } else "Stable"

      tibble::tibble(
        presence         = list(x_ts$presence),
        total_catch      = list(x_ts$harvest_a_b1_numbers),
        catch_trend      = catch_msg,
        harvest_fraction = list(x_ts$harvest_frac),
        fract_trend      = fract_msg
      )

    }, .id = "var_area") |>
      tidyr::separate(var_area, into = c("region", "state"), sep = "_") |>
      dplyr::mutate(
        state  = factor(state,  levels = states_ns),
        region = factor(region, levels = region_levels),
        catch_dir = dplyr::recode_values(
          catch_trend,
          "Stable"       ~ "minus",
          "Increasing"   ~ "arrow-up",
          "Decreasing"   ~ "arrow-down",
          "Limited Data" ~ NA_character_),
        fract_dir = dplyr::recode_values(
          fract_trend,
          "Stable"       ~ "minus",
          "Increasing"   ~ "arrow-up",
          "Decreasing"   ~ "arrow-down",
          "Limited Data" ~ NA_character_)
      ) |>
      dplyr::arrange(region, state)
  })

  #return(shortlist_trends)

  out <- shortlist_trends |>
    purrr::imap(function(trends_x, species_x){

      # Make the table on proportions only
      trends_table <- trends_x |>
        dplyr::arrange(region, state) |>
        gt::gt(groupname_col = "region") |>
        gt::tab_header(
          title = Hmisc::html(
            stringr::str_c(
              "Species of Interest: ",
              stringr::str_to_title(species_x)
            )),
          subtitle =  "MRIP Recreational Catch Trends of the Last 15 Years")  |>
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
            # "arrow-up" = "seagreen",
            # "arrow-down" = "tomato"
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
        # # enforce the order groups
        # row_group_order(
        #   groups = region_levels[1:3]) |>
        gt::opt_table_font(font = list(gt::google_font(name = "Avenir")))

      return(trends_table)

    })
  if (species == "all") {
    return(out |> dplyr::select(species_x, trends_table))
  } else {
    return(out)
  }
}
test <- plot_catch_trends(species = "summer flounder", catch_data = catch)

