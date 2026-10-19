# Import country-year economic satisfaction summaries and reliability from ESS.
read_ess_country_years <- function() {
  ess <- read.csv(project_path(paths$ess_csv), check.names = FALSE)
  ess <- ess[
    ess$variable == "stfeco" & ess$unit_type == "country_year" &
      !is.na(ess$country) & ess$country != "All", ,
    drop = FALSE
  ]
  numeric_columns <- c(
    "mean", "median", "std", "p10", "p90", "iqr", "n_effective"
  )
  ess[numeric_columns] <- lapply(ess[numeric_columns], function(column) {
    suppressWarnings(as.numeric(column))
  })
  ess$year <- as.integer(ess$year)
  ess$reliable <- tolower(as.character(ess$reliable)) %in% "true"
  ess[c("country", "year", numeric_columns, "reliable")]
}

# Convert CWS country IDs and CPI2 observations to country-year records.
prepare_cws_cpi2 <- function(data) {
  required <- c("id", "year", "cpi2")
  if (!all(required %in% names(data))) {
    stop("CWS data must contain id, year, and cpi2 columns.")
  }
  country_names <- c(
    AUL = "Australia", AUS = "Austria", BEL = "Belgium", CAN = "Canada",
    DEN = "Denmark", FIN = "Finland", FRA = "France", FRG = "Germany",
    GRE = "Greece", IRE = "Ireland", ITA = "Italy", JPN = "Japan",
    LUX = "Luxembourg", NET = "Netherlands", NOR = "Norway",
    NZL = "New Zealand", POR = "Portugal", SPA = "Spain", SWE = "Sweden",
    SWZ = "Switzerland", UKM = "United Kingdom", USA = "United States"
  )
  cpi2 <- suppressWarnings(as.numeric(data$cpi2))
  year <- suppressWarnings(as.integer(as.numeric(data$year)))
  country <- unname(country_names[as.character(data$id)])
  usable <- is.finite(cpi2)
  if (any(usable & (is.na(country) | is.na(year)))) {
    stop("CWS CPI2 contains an unmapped country ID or invalid year.")
  }
  result <- data.frame(country = country[usable], year = year[usable], cpi2 = cpi2[usable])
  if (anyDuplicated(result[c("country", "year")])) {
    stop("CWS CPI2 contains duplicate country-year observations.")
  }
  result
}

# Import CPI2 from the CWS workbook and normalize its country-year keys.
read_cws_cpi2 <- function() {
  file <- project_path(paths$cws_xlsx)
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Reading CWS CPI2 requires the readxl R package.")
  }
  data <- readxl::read_excel(file, .name_repair = "minimal")
  prepare_cws_cpi2(data)
}

# Resolve a WID country's data or metadata file within the raw-data directory.
wid_file <- function(code, kind) {
  project_path(paths$wid_dir, sprintf("WID_%s_%s.csv", kind, code))
}

# Read WID's semicolon-delimited exports without modifying their column names.
read_wid_csv <- function(file, ...) {
  read.csv(file, sep = ";", check.names = FALSE, ...)
}

# Import the WID country-name and region-code lookup.
read_wid_countries <- function() {
  read_wid_csv(project_path(paths$wid_dir, "WID_countries.csv"))
}

# Import series definitions and extrapolation metadata for a WID country.
read_wid_metadata <- function(code) read_wid_csv(wid_file(code, "metadata"))

# Import a WID country's observations with fixed numeric year and value types.
read_wid_data <- function(code) {
  read_wid_csv(
    wid_file(code, "data"),
    colClasses = c(
      "character", "character", "character", "integer",
      "numeric", "character", "character"
    )
  )
}

# List source files whose changes invalidate cached analysis outputs.
raw_input_files <- function() {
  c(
    project_path(paths$ess_csv),
    project_path(paths$cws_xlsx),
    list.files(
      project_path(paths$wid_dir),
      pattern = "^WID_(countries|data_.+|metadata_.+)\\.csv$",
      full.names = TRUE
    )
  )
}
