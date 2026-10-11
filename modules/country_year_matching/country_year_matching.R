# Find an equal-split adult share series matching the requested distribution concept.
find_share_variable <- function(metadata, concept_pattern) {
  rows <- metadata[
    metadata$age == "992" & metadata$pop == "j" &
      tolower(metadata$shorttype) == "share" &
      grepl(concept_pattern, metadata$simpledes, ignore.case = TRUE), ,
    drop = FALSE
  ]
  if (nrow(rows)) rows$variable[[1L]] else NA_character_
}

# Prefer post-tax national income, falling back to the available pre-tax series.
select_wid_income_series <- function(metadata) {
  for (basis in names(wid_income_candidates)) {
    variable <- find_share_variable(metadata, wid_income_candidates[[basis]])
    if (!is.na(variable)) {
      return(list(
        variable = variable, basis = basis,
        label = wid_income_candidates[[basis]]
      ))
    }
  }
  NULL
}

# Import the selected income/wealth adult decile shares from a WID country file.
read_wid_decile_shares <- function(code, variables) {
  wid <- read_wid_data(code)
  wid <- wid[
    wid$variable %in% variables & wid$age == "992" & wid$pop == "j" &
      wid$percentile %in% decile_codes & !is.na(wid$value), ,
    drop = FALSE
  ]
  wid$concept <- names(variables)[match(wid$variable, variables)]
  wid[c("concept", "percentile", "year", "value")]
}

# Returns the first WID region code with ESS years covered by every WID concept.
match_country_with_wid <- function(country, ess_years, wid_countries) {
  wid_name <- if (country %in% names(country_aliases)) {
    country_aliases[[country]]
  } else {
    country
  }
  candidates <- wid_countries$alpha2[
    tolower(wid_countries$shortname) == tolower(wid_name)
  ]

  for (code in candidates) {
    if (!all(file.exists(wid_file(code, c("metadata", "data"))))) next

    metadata <- read_wid_metadata(code)
    wealth_variable <- find_share_variable(metadata, wid_wealth_concept)
    income_series <- select_wid_income_series(metadata)
    if (is.na(wealth_variable) || is.null(income_series)) next
    variables <- c(wealth = wealth_variable, income = income_series$variable)

    wid <- read_wid_decile_shares(code, variables)
    years_by_concept <- lapply(names(variables), function(concept) {
      wid$year[wid$concept == concept]
    })
    common_years <- sort(Reduce(intersect, c(list(ess_years), years_by_concept)))
    if (!length(common_years)) next

    return(list(
      country = country,
      wid_code = code,
      years = common_years,
      change_years = select_survey_round_years(common_years),
      variables = variables,
      income_basis = income_series$basis,
      income_label = income_series$label,
      extrapolation = wid_extrapolation_intervals(metadata, variables),
      wid = wid[wid$year %in% common_years, , drop = FALSE]
    ))
  }
  NULL
}

# Match survey countries and years to both income and wealth distributions.
compute_country_year_matching <- function() {
  ess <- read_ess_country_years()
  wid_countries <- read_wid_countries()

  years_by_country <- split(ess$year, ess$country)
  matches <- Map(
    function(years, country) match_country_with_wid(country, years, wid_countries),
    years_by_country, names(years_by_country)
  )
  matches <- Filter(Negate(is.null), matches)
  if (!length(matches)) {
    warning("No country-years were found in both ESS and WID for all concepts.")
  }

  country_years <- bind_rows(lapply(matches, function(match) {
    data.frame(country = match$country, wid_code = match$wid_code, year = match$years)
  }))
  list(ess = ess, matches = matches, country_years = country_years)
}

# Shared by all feature modules; changes to raw data or matching code invalidate it.
run_country_year_matching <- function() {
  cached_result(
    "country_year_matching",
    input_files = raw_input_files(),
    code_files = code_files_for("matching"),
    compute = compute_country_year_matching
  )
}
