read_ess_country_years <- function() {
  ess <- read.csv(
    project_path(paths$ess_csv),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  ess <- ess[
    ess$variable == "stfeco" & ess$unit_type == "country_year" &
      !is.na(ess$country) & ess$country != "All",
  ]
  ess$year <- as.integer(ess$year)
  for (column in c("mean", "median", "mode", "iqr")) {
    ess[[column]] <- suppressWarnings(as.numeric(ess[[column]]))
  }
  ess$reliable <- tolower(as.character(ess$reliable)) == "true"
  ess[, c("country", "year", "mean", "median", "mode", "iqr", "reliable")]
}

wid_file <- function(code, kind) {
  project_path(paths$wid_dir, sprintf("WID_%s_%s.csv", kind, code))
}

read_wid_countries <- function() {
  read.csv(
    project_path(paths$wid_dir, "WID_countries.csv"),
    sep = ";",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

read_wid_metadata <- function(code) {
  read.csv(
    wid_file(code, "metadata"),
    sep = ";",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

read_wid_data <- function(code) {
  read.csv(
    wid_file(code, "data"),
    sep = ";",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = c(
      "character", "character", "character", "integer",
      "numeric", "character", "character"
    )
  )
}

wid_input_files <- function() {
  list.files(
    project_path(paths$wid_dir),
    pattern = "^WID_(countries|data_.+|metadata_.+)\\.csv$",
    full.names = TRUE
  )
}

ess_input_files <- function() project_path(paths$ess_csv)

raw_input_files <- function() c(ess_input_files(), wid_input_files())
