### GARFO Federal Permits Data ###
# box_path  <- "/Users/clovas/Library/CloudStorage/Box-Box/MAFMC-25 Data/"
# proj_path <- paste0(box_path, "Non-confidential/Permits/")

#' @title Pull federal permits data
#'
#' @description Function to pull and clean GARFO commercial fishing permits from pre-existing confidential repository.
#'
#' @param proj_path Local path to data file
#' @return Data frame of permits; includes year, prinicpal port and state, permit type, target species, and category (commerical, for-hire).
#' @export
#' @examples # permits <- pull_permits(proj_path = proj_path)
pull_permits <- function(proj_path){

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
      stringr::str_starts(permit, "red crab") ~ "red crab",
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
  out <- permits |>
    dplyr::filter(!target == "squid/mackerel/butterfish") |>
    dplyr::full_join(smb)


  return(out)

}

ports <- permits |>
    dplyr::select(pport, ppst) |>
    dplyr::distinct() |>
    tidygeocoder::geocode(city  = pport, state = ppst)


clean <- ports |>
  tidyr::drop_na() |>
  dplyr::arrange(ppst) |>
  dplyr::mutate(port = paste(pport, ppst, sep = ", ")) |>
  dplyr::select(port, lat, long)

na <- ports |>
  dplyr::filter(is.na(lat)) |>
  dplyr::mutate(port = paste(pport, ppst, sep = ", ")) |>
  dplyr::select(port)

test <- na |>
  # dplyr::select(pport,ppst) |>
  fuzzyjoin::stringdist_left_join(clean)

