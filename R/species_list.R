## List of common names across data sets
### MAFMC managed species have different common names across different data sets. This will combine them to make selecting species when running speciesshifts functions.

#' @title Mid-Atlantic Species
#'
#' @description Raw and cleaned common names for species managed by the MAFMC across six federal data sets.
#'
#' @param source Filter to desired data set. source = "all" returns complete list. Options include "nefsc, vtr, observer, landings, mrip, and permits"
#' @return Data frame of landings by species, principal port and state, landings, live weight, value, latitude and longitude of principal port.
#' @export
#'
#' @examples species_list(source = "all")
#'
species_list <- function(source = "all") {

  data_sources <- c("all", "landings", "mrip", "observer", "permits", "nefsc", "vtr")
  if (!source %in% data_sources) {
    message("Invalid source '", source, "'. Choose from: ", paste(data_sources, collapse = ", "))
    return(NULL)
  }

  mrip <- data.frame(comname = c(
    "atlantic mackerel", "black sea bass", "bluefish", "blueline tilefish", "butterfish",
    "chub mackerel", "king mackerel", "monkfish", "scup", "spiny dogfish", "summer flounder",
    "tilefish", "atlantic croaker", "striped bass", "gray triggerfish", "spanish mackerel"
  )) |>
    dplyr::mutate(clean_name = comname,
                  data_source = "mrip")

  landings <- data.frame(comname = c(
    "atlantic mackerel", "black sea bass", "bluefish", "blueline tilefish", "butterfish",
    "chub mackerel", "goosefish", "king mackerel", "longfin/loligo squid", "ocean clam, quahog",
    "scup", "shortfin/illex squid", "spiny dogfish", "summer flounder", "surf clam", "tilefish",
    "atlantic croaker", "atlantic striped bass", "gray triggerfish", "spanish mackerel"
  )) |>
    dplyr::mutate(
      clean_name = dplyr::case_when(
        comname == "longfin/loligo squid"  ~ "longfin squid",
        comname == "shortfin/illex squid"  ~ "northern shortfin squid",
        comname == "ocean clam, quahog"    ~ "ocean quahog",
        comname == "goosefish"             ~ "monkfish",
        .default = comname
      ),
      data_source = "landings"
    )

  vtr <- data.frame(comname = c(
    "mackerel, atlantic", "mackerel, chub", "croaker, atlantic", "striped bass", "black sea bass",
    "tilefish", "bluefish", "butterfish", "monkfish / anglerfish / goosefish", "scup / porgy",
    "dogfish, spiny", "flounder, summer / fluke", "tilefish, blueline", "mackerel, spanish",
    "mackerel, king", "ocean quahog", "clam, surf", "squid / loligo", "squid / illex"
  )) |>
    dplyr::mutate(
      clean_name = stringr::str_remove(comname, pattern = " /.*"),
      clean_name = SwimmeR::name_reorder(clean_name),
      clean_name = dplyr::case_when(
        comname == "squid / illex"  ~ "northern shortfin squid",
        comname == "squid / loligo" ~ "longfin squid",
        .default = clean_name
      ),
      data_source = "vtr"
    )

  trawl <- data.frame(comname = c(
    "atlantic mackerel", "atlantic surfclam", "black sea bass", "bluefish", "blueline tilefish",
    "butterfish", "chub mackerel", "king mackerel", "longfin squid", "goosefish",
    "northern shortfin squid", "ocean quahog", "scup", "spiny dogfish", "summer flounder",
    "tilefish", "atlantic croaker", "striped bass", "gray triggerfish", "spanish mackerel"
  )) |>
    dplyr::mutate(clean_name = ifelse(comname == "goosefish", "monkfish", comname),
                  data_source = "nefsc")

  observer <- data.frame(comname = c(
    "mackerel, atlantic", "mackerel, chub, atlantic", "croaker, atlantic", "bass, striped",
    "sea bass, black", "tilefish, golden", "bluefish", "butterfish", "monkfish (goosefish)",
    "scup", "dogfish, spiny", "flounder, summer (fluke)", "tilefish, blueline", "mackerel, spanish",
    "mackerel, king", "quahog, ocean (black clam)", "squid, longfin, atlantic", "squid, shortfin"
  )) |>
    dplyr::mutate(
      clean_name = stringr::str_remove(comname, pattern = " \\(.*"),
      clean_name = SwimmeR::name_reorder(clean_name),
      clean_name = dplyr::case_when(
        comname == "mackerel, chub, atlantic"  ~ "chub mackerel",
        comname == "squid, longfin, atlantic"  ~ "longfin squid",
        comname == "squid, shortfin"           ~ "northern shortfin squid",
        comname == "tilefish, golden"          ~ "tilefish",
        .default = clean_name
      ),
      data_source = "observer"
    )

  permits <- data.frame(comname = c(
    "atlantic mackerel", "black sea bass", "bluefish", "blueline tilefish", "butterfish",
    "chub mackerel", "monkfish", "king mackerel", "longfin squid", "ocean quahog", "scup",
    "northern shortfin squid", "dogfish", "summer flounder", "surf clam", "tilefish"
  )) |>
    dplyr::mutate(clean_name = comname,
                  data_source = "permits")

  combined <- dplyr::bind_rows(landings, mrip, observer, permits, trawl, vtr)

  if (source != "all") {
    out <- combined |> dplyr::filter(data_source == source)
  } else {
    out <- combined
  }

  return(out)
}

