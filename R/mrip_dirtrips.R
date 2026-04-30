#' Extract MRIP Directed Trip Estimates for Multiple Species
#'
#' Downloads and processes MRIP intercept survey data to estimate directed
#' fishing trips by species, state, mode, and area along the US East Coast.
#'
#' @param species Character vector of species common names (uppercase).
#'   E.g., \code{c("STRIPED BASS", "BLUEFISH")}.
#' @param states Integer vector of FIPS-style MRIP state codes.
#'   Defaults to East Coast states: ME, NH, MA, RI, CT, DE, MD, NJ, NY, VA,
#'   NC, SC, GA, and FL (east coast).
#' @param start_year Integer. First year of data to retrieve. Default \code{2010}.
#' @param end_year Integer. Last year of data to retrieve. Default \code{2024}.
#' @param trip_type Integer scalar (1--5). Trip definition passed to
#'   \code{MRIP.dirtrips()}. \code{1} = primary target (default).
#' @param domain List defining domain strata passed to \code{MRIP.dirtrips()}.
#'   Defaults to all six waves: \code{list(wave = list(c(1:6)))}.
#' @param data_dir Path to the directory where raw MRIP zip files and extracted
#'   CSVs will be stored. Defaults to \code{here::here("data", "MRIP_Index")}.
#' @param output_path Path for the processed CSV output file. Set to
#'   \code{NULL} to skip writing. Defaults to
#'   \code{here::here("data", "processed", "mrip_primary_target.csv")}.
#' @param download Logical. If \code{TRUE} (default), scrape NMFS and download
#'   any missing zip files before processing.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per species-domain
#'   combination, containing trip estimates, standard errors, PSEs, and
#'   human-readable \code{state}, \code{mode}, and \code{area} columns.
#'   Invisibly also writes a CSV when \code{output_path} is not \code{NULL}.
#'
#' @details
#' The function performs three steps:
#' \enumerate{
#'   \item \strong{Download}: Scrapes the NMFS MRIP CSV directory for
#'     \code{ps_*.zip} files and downloads any that are missing locally.
#'   \item \strong{Organise}: Moves extracted CSVs into \code{int<YYYY>}
#'     sub-folders expected by \code{MRIP.dirtrips()}.
#'   \item \strong{Estimate}: Calls \code{MRIP.dirtrips()} once per species,
#'     row-binds the results, and attaches readable labels for state, fishing
#'     mode, and area.
#' }
#'
#' Processing all East Coast states over many years can take several hours.
#'
#' @examples
#' \dontrun{
#' # Quick run: two species, Mid-Atlantic only
#' mid_atl <- c(10, 24, 34, 36, 51)
#'
#' trips <- get_mrip_directed_trips(
#'   species    = c("STRIPED BASS", "SUMMER FLOUNDER"),
#'   states     = mid_atl,
#'   start_year = 2018,
#'   end_year   = 2022
#' )
#'
#' # Full East Coast run (warning: ~4 hours)
#' trips_all <- get_mrip_directed_trips(
#'   species = c("BLUEFISH", "BLACK SEA BASS", "SCUP")
#' )
#' }
#'
#' @importFrom purrr map set_names
#' @importFrom data.table rbindlist
#' @importFrom dplyr select mutate case_when
#' @importFrom readr write_csv
#' @importFrom here here
#' @import survey
#' @export
pull_mrip_directed_trips <- function(
    species    = c(
      "ATLANTIC CROAKER", "ATLANTIC MACKEREL", "BLACK SEA BASS",
      "SPANISH MACKEREL", "STRIPED BASS", "SUMMER FLOUNDER",
      "SCUP", "SPINY DOGFISH", "GOOSEFISH", "TILEFISH",
      "BLUELINE TILEFISH", "BLUEFISH", "GRAY TRIGGERFISH", "KING MACKEREL"
    ),
    states     = c(23, 33, 25, 44, 9, 10, 24, 34, 36, 51, 37, 45, 13, 121),
    start_year = 2010,
    end_year   = 2024,
    trip_type  = 1,
    domain     = list(wave = list(c(1, 2, 3, 4, 5, 6))),
    data_dir   = here::here("data", "MRIP_Index"),
    # output_path = here::here("data", "processed", "mrip_primary_target.csv"),
    download   = TRUE
) {

  # ---- Input validation --------------------------------------------------- #
  stopifnot(
    is.character(species), length(species) >= 1,
    is.numeric(states),    length(states)  >= 1,
    is.numeric(start_year), length(start_year) == 1,
    is.numeric(end_year),   length(end_year)   == 1,
    start_year <= end_year,
    length(trip_type) == 1
  )

  # ---- Step 1: Download MRIP zip files ------------------------------------ #
  if (download) {
    .mrip_download(data_dir)
  }

  # ---- Step 2: Organise extracted CSVs into int<YYYY> folders ------------- #
  .mrip_organise(data_dir)

  # ---- Step 3: Estimate directed trips ------------------------------------ #
  message("Estimating directed trips for ", length(species), " species ...")

  out <- species |>
    purrr::set_names() |>
    purrr::map(function(sp) {
      message("  Processing: ", sp)
      MRIP.dirtrips(
        intdir = data_dir,
        common = sp,
        st     = states,
        styr   = start_year,
        endyr  = end_year,
        trips  = trip_type,
        dom    = domain
      )
    }) |>
    data.table::rbindlist(idcol = "species_name", fill = TRUE) |>
    dplyr::select(-species_name) |>
    dplyr::mutate(
      state = .mrip_state_labels(st),
      mode  = .mrip_mode_labels(mode_fx),
      area  = .mrip_area_labels(area_x)
    )

  # ---- Step 4: (Optional) write output ------------------------------------ #
  # if (!is.null(output_path)) {
  #   dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  #   readr::write_csv(out, output_path)
  #   message("Results written to: ", output_path)
  # }
  #
  # invisible(out)
}


# ---- Internal helpers ------------------------------------------------------- #

#' @keywords internal
.mrip_download <- function(data_dir) {
  base_url <- "https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/CSV/"
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  message("Scraping MRIP directory for zip files ...")
  page     <- readLines(base_url, warn = FALSE)
  zip_names <- unique(unlist(regmatches(page, gregexpr("ps_[^\"]+\\.zip", page))))

  if (length(zip_names) == 0) {
    warning("No ps_*.zip files found at ", base_url, ". Check URL or network.")
    return(invisible(NULL))
  }

  for (zn in zip_names) {
    destfile <- file.path(data_dir, zn)
    if (!file.exists(destfile)) {
      message("  Downloading ", zn, " ...")
      utils::download.file(paste0(base_url, zn), destfile, mode = "wb", quiet = TRUE)
    }
    message("  Extracting ", zn, " ...")
    utils::unzip(destfile, exdir = data_dir)
  }

  invisible(NULL)
}


#' @keywords internal
.mrip_organise <- function(data_dir) {
  all_csvs <- list.files(
    data_dir, pattern = "\\.(csv|CSV)$",
    recursive = TRUE, full.names = TRUE
  )

  for (f in all_csvs) {
    yr_match <- regmatches(basename(f), regexpr("\\d{4}", basename(f)))
    if (length(yr_match) != 1) next

    int_dir <- file.path(data_dir, paste0("int", yr_match))
    dir.create(int_dir, showWarnings = FALSE)

    dest <- file.path(int_dir, basename(f))
    if (!file.exists(dest)) {
      file.rename(f, dest)
      message("  Moved ", basename(f), " -> int", yr_match, "/")
    }
  }

  invisible(NULL)
}

#' @keywords internal
MRIP.dirtrips <- function(intdir = NULL, common = NULL, st = NULL, styr = NULL,
                          endyr = NULL, trips = 1, dom = NULL) {
  # Gary Nelson, Massachusetts Division of Marine Fisheries
  # gary.nelson@mass.gov
  # 07_22_2022 - G Nelson coded method to extract domain labels for better output

  # --- Input validation ---
  if (is.null(intdir)) stop("Need main directory location of intercept files.")
  if (is.null(common))  stop("Need Common Name for common.")
  if (is.null(st))      stop("No state code(s) was specified.")
  if (is.null(styr))    stop("Starting year is missing.")
  if (is.null(endyr))   stop("Ending year is missing.")
  if (length(trips) != 1) stop("Only one trip option can be used at a time for accurate calculations.")

  # --- Check that required packages are available ---
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package 'survey' is required. Install it with: install.packages('survey')")
  }

  # --- Build input directory path ---
  if (length(grep("/", intdir)) == 1) {
    din <- ifelse(
      substr(intdir, nchar(intdir), nchar(intdir)) %in% "/",
      paste0(intdir, "int"),
      paste0(intdir, "/int")
    )
  }
  if (length(grep("\\\\", intdir)) == 1) {
    din <- ifelse(
      substr(intdir, nchar(intdir), nchar(intdir)) %in% "\\",
      paste0(intdir, "int"),
      paste0(intdir, "\\int")
    )
  }

  # --- Normalise inputs ---
  common <- tolower(common)
  st     <- as.character(st)
  styr   <- as.character(styr)
  endyr  <- as.character(endyr)
  wave   <- as.character(1:6)

  # --- Helper: safe rbind that keeps only shared columns ---
  rbind2 <- function(input1, input2) {
    if (!is.null(ncol(input1))) {
      n.input1 <- ncol(input1)
      n.input2 <- ncol(input2)
      if (n.input2 < n.input1) {
        TF.names     <- which(names(input2) %in% names(input1))
        column.names <- names(input2[, TF.names])
      } else {
        TF.names     <- which(names(input1) %in% names(input2))
        column.names <- names(input1[, TF.names])
      }
      return(rbind(input1[, column.names], input2[, column.names]))
    }
    if (is.null(ncol(input1))) return(rbind(input1, input2))
  }

  # --- Helper: convert all columns to lower case ---
  convtolow <- function(x) {
    for (i in 1:ncol(x)) x[, i] <- tolower(x[, i])
    return(x)
  }

  # --- Read and stack catch and trip files ---
  temp  <- NULL
  temp1 <- NULL

  for (yr in styr:endyr) {
    for (wv in wave) {
      # Catch file
      t3 <- utils::read.csv(
        paste0(din, yr, "/catch_", yr, wv, ".csv"),
        colClasses = "character", na.strings = "."
      )
      t3 <- t3[t3$ST %in% st, ]
      names(t3) <- tolower(names(t3))
      temp <- rbind2(temp, t3)

      # Trip file
      t4 <- utils::read.csv(
        paste0(din, yr, "/trip_", yr, wv, ".csv"),
        colClasses = "character", na.strings = "."
      )
      t4 <- t4[t4$ST %in% st, ]
      names(t4) <- tolower(names(t4))
      temp1 <- rbind2(temp1, t4)
    }
  }

  temp  <- convtolow(temp)
  temp1 <- convtolow(temp1)

  # --- Retain only required catch columns and sort ---
  temp <- temp[, c("common", "strat_id", "psu_id", "st", "id_code", "sp_code",
                   "claim", "release", "harvest", "tot_len_a", "wgt_a",
                   "tot_len_b1", "wgt_b1", "fl_reg", "tot_cat",
                   "wgt_ab1", "tot_len", "landing")]
  temp  <- temp[order(temp$strat_id,   temp$psu_id,  temp$id_code), ]
  temp1 <- temp1[order(temp1$strat_id, temp1$psu_id, temp1$id_code), ]

  # --- Merge catch onto trips ---
  dataset <- merge(
    temp1, temp,
    by    = c("strat_id", "psu_id", "id_code", "st"),
    all.x = FALSE, all.y = FALSE
  )
  dataset$common <- as.character(dataset$common)
  dataset$common <- ifelse(is.na(dataset$common), "", dataset$common)

  if (!any(dataset$common == common)) stop("common not found.")

  # --- Construct domain ID ---
  dom_ids <- NULL
  mainlev <- length(dom)

  if (mainlev > 0) {
    for (l in 1:mainlev) {
      varname <- names(dom)[l]
      if (!any(varname == names(dataset)))
        stop(paste("Variable", varname, "not found in MRIP dataset"))

      newcol <- paste0(varname, "1")
      dataset[[newcol]] <- "DELETE"
      colpos  <- which(names(dataset) == varname)
      sublev  <- length(dom[[l]])

      for (k in 1:sublev) {
        dataset[[newcol]] <- ifelse(
          dataset[[colpos]] %in% as.character(dom[[l]][[k]]),
          paste0(varname, k),
          dataset[[newcol]]
        )
      }
      dom_ids <- c(dom_ids, varname)
    }

    test <- c("year", "wave", "st", "sub_reg", "mode_fx", "area_x")
    for (gg in seq_along(dom_ids)) {
      if (!any(dom_ids[gg] == test)) {
        test <- c(test, paste0(dom_ids[gg], "1"))
      } else {
        colpos       <- which(test == dom_ids[gg])
        test[colpos] <- paste0(dom_ids[gg], "1")
      }
    }

    # Prefix non-domain column values with the column name
    for (gg in seq_along(test)) {
      if (substr(test[gg], nchar(test[gg]), nchar(test[gg])) != "1") {
        dataset[[test[gg]]] <- paste0(test[gg], dataset[[test[gg]]])
      }
    }

    # Build composite domain ID string
    dataset$dom_id <- do.call(paste0, dataset[, test])
    tempdom <- paste(paste0("dataset$", test), collapse = ",")

  } else {
    # No custom domain: use default fields
    dataset$year    <- paste0("year",    dataset$year)
    dataset$wave    <- paste0("wave",    dataset$wave)
    dataset$st      <- paste0("st",      dataset$st)
    dataset$sub_reg <- paste0("sub_reg", dataset$sub_reg)
    dataset$mode_fx <- paste0("mode_fx", dataset$mode_fx)
    dataset$area_x  <- paste0("area_x",  dataset$area_x)
    dataset$dom_id  <- paste0(dataset$year, dataset$wave, dataset$st,
                              dataset$sub_reg, dataset$mode_fx, dataset$area_x)
    tempdom <- "dataset$year,dataset$wave,dataset$st,dataset$sub_reg,dataset$mode_fx,dataset$area_x"
  }

  # --- trips == 3: reassign claim across grouped catch records ---
  if (trips == 3) {
    subset   <- dataset[dataset$common == common, ]
    maxclaim <- stats::aggregate(claim ~ leader, subset, max)
    combined <- merge(dataset, maxclaim, by = "leader", all = TRUE)

    dataset  <- dataset[order(dataset$leader), ]
    combined <- combined[order(combined$leader), ]
    dataset$claim <- combined$claim.y

    sub1          <- dataset[as.numeric(dataset$claim) > 0 & dataset$common != common, ]
    sub2          <- dataset[!(as.numeric(dataset$claim) > 0 & dataset$common != common), ]
    sub1$common   <- common
    dataset       <- rbind(sub1, sub2)
  }

  # --- Coerce numeric columns ---
  for (col in c("tot_cat", "landing", "claim", "harvest", "release", "wgt_ab1", "wp_int")) {
    dataset[[col]] <- as.numeric(dataset[[col]])
  }

  # --- Species-specific catch columns ---
  dataset$dcomm    <- common
  dataset$dtotcat  <- ifelse(dataset$common == common, dataset$tot_cat,  0)
  dataset$dlandings<- ifelse(dataset$common == common, dataset$landing,  0)
  dataset$dclaim   <- ifelse(dataset$common == common, dataset$claim,    0)
  dataset$dharvest <- ifelse(dataset$common == common, dataset$harvest,  0)
  dataset$drelease <- ifelse(dataset$common == common, dataset$release,  0)
  dataset$dwgt_ab1 <- ifelse(dataset$common == common, dataset$wgt_ab1,  0)

  # --- Build domain filter condition string for trips options 1-5 ---
  temlab <- NULL
  cnt    <- 0
  trip_conditions <- list(
    `1` = "dataset$prim1_common==common",
    `2` = "dataset$prim2_common==common",
    `3` = "dataset$common==common & as.numeric(dataset$claim)>0",
    `4` = "dataset$common==common & as.numeric(dataset$harvest)>0",
    `5` = "dataset$common==common & as.numeric(dataset$release)>0"
  )
  for (i in 1:5) {
    if (any(trips == i)) {
      cnt    <- cnt + 1
      sep    <- if (cnt == 1) "" else " & "
      temlab <- paste0(temlab, sep, trip_conditions[[as.character(i)]])
    }
  }

  dataset$dom_id <- eval(parse(text = paste0(
    "ifelse(", temlab, ", dataset$dom_id, 2)"
  )))

  # --- Aggregate to PSU level ---
  dataset1 <- stats::aggregate(
    cbind(dataset$dtotcat, dataset$dlandings, dataset$dclaim,
          dataset$dharvest, dataset$drelease, dataset$dwgt_ab1),
    list(dataset$strat_id, dataset$psu_id, dataset$id_code, dataset$wp_int,
         dataset$prim1_common, dataset$prim2_common, dataset$dcomm, dataset$dom_id),
    sum
  )
  names(dataset1) <- c(
    "strat_id", "psu_id", "id_code", "wp_int",
    "prim1_common", "prim2_common", "common", "dom_id_add",
    "total.catch", "harvest.A.B1", "claim.A", "reported.B1",
    "released.B2", "weight"
  )
  dataset1$dtrip <- 1

  # --- Survey-weighted totals ---
  options(survey.lonely.psu = "certainty")

  dfpc <- survey::svydesign(
    ids     = ~psu_id,
    strata  = ~strat_id,
    weights = ~wp_int,
    nest    = TRUE,
    data    = dataset1
  )

  results <- survey::svyby(
    ~dtrip, ~dom_id_add, dfpc, survey::svytotal,
    vartype    = c("se", "cv"),
    keep.names = FALSE
  )
  names(results) <- c("Domain", "Trips", "SE", "PSE")

  # --- Remove placeholder / null domains ---
  if (length(grep("DELETE", results$Domain, fixed = TRUE)) > 0)
    results <- results[-grep("DELETE", results$Domain, fixed = TRUE), ]
  results <- results[results$Domain != "2", ]
  results <- results[order(results$Domain), ]

  if (nrow(results) == 0) return(results)

  # --- Build readable domain labels ---
  tempdom <- gsub(
    "([0-9]+).*$", "",
    unlist(strsplit(gsub("dataset$", "", tempdom, fixed = TRUE), ","))
  )

  for (i in seq_along(tempdom)) {
    assign(paste0(tempdom[i], "_first_pos"),
           regexpr(pattern = tempdom[i], results$Domain))
    fp  <- get(paste0(tempdom[i], "_first_pos"))
    assign(paste0(tempdom[i], "_label"),
           substr(results$Domain,
                  start = fp,
                  stop  = attributes(fp)$match.length + fp - 1))
  }

  for (i in 1:(length(tempdom) - 1)) {
    fp_cur  <- get(paste0(tempdom[i],     "_first_pos"))
    fp_next <- get(paste0(tempdom[i + 1], "_first_pos"))
    assign(paste0(tempdom[i], "_value"),
           substr(results$Domain,
                  start = attributes(fp_cur)$match.length + fp_cur,
                  stop  = fp_next - 1))
  }

  # Last domain element value
  foo <- gregexpr("([[:digit:]]+)", results$Domain)
  start_positions    <- NULL
  length_last_numeric <- NULL
  for (y in seq_along(foo)) {
    endval              <- length(foo[[y]])
    start_positions     <- c(start_positions,     foo[[y]][endval])
    length_last_numeric <- c(length_last_numeric, attributes(foo[[y]])$match.length[endval])
  }
  last_pos <- start_positions + length_last_numeric - 1
  assign(paste0(tempdom[length(tempdom)], "_value"),
         substr(results$Domain, start = start_positions, stop = last_pos))

  # --- Assemble output data frame ---
  foo1 <- data.frame(get(paste0(tempdom[1], "_value")), stringsAsFactors = FALSE)
  names(foo1)[1] <- tempdom[1]
  for (i in 2:length(tempdom)) {
    foo1[[tempdom[i]]] <- get(paste0(tempdom[i], "_value"))
  }
  foo1$species <- common

  outpt      <- cbind(foo1, results[, -1])
  outpt$PSE  <- round(outpt$PSE * 100, 1)
  return(outpt)
}


#' @keywords internal
.mrip_state_labels <- function(st) {
  dplyr::case_when(
    st == "23"  ~ "ME",
    st == "33"  ~ "NH",
    st == "25"  ~ "MA",
    st == "44"  ~ "RI",
    st == "9"   ~ "CT",
    st == "10"  ~ "DE",
    st == "24"  ~ "MD",
    st == "34"  ~ "NJ",
    st == "36"  ~ "NY",
    st == "51"  ~ "VA",
    st == "37"  ~ "NC",
    st == "45"  ~ "SC",
    st == "13"  ~ "GA",
    st == "121" ~ "FL",
    .default    =  NA_character_
  )
}


#' @keywords internal
.mrip_mode_labels <- function(mode_fx) {
  dplyr::case_when(
    mode_fx == "1" ~ "Man-Made",
    mode_fx == "2" ~ "Beach/Bank",
    mode_fx == "3" ~ "Shore",
    mode_fx == "4" ~ "Head Boat",
    mode_fx == "5" ~ "Charter Boat",
    mode_fx == "7" ~ "Private & Rental",
    .default       =  NA_character_
  )
}


#' @keywords internal
.mrip_area_labels <- function(area_x) {
  dplyr::case_when(
    area_x == "1" ~ "State Territorial Seas (Ocean<=3 mi excluding Inland)",
    area_x == "2" ~ "Exclusive Economic Zone (Ocean>3 mi)",
    area_x == "3" ~ "Ocean <=10 mi West FL and TX",
    area_x == "4" ~ "Ocean > 10 mi West FL and TX",
    area_x == "5" ~ "Inland",
    area_x == "6" ~ "Unknown",
    .default       =  NA_character_
  )
}

