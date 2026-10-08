find_share_variable <- function(metadata, concept_pattern) {
  rows <- metadata[
    metadata$age == "992" & metadata$pop == "j" &
      tolower(metadata$shorttype) == "share" &
      grepl(concept_pattern, metadata$simpledes, ignore.case = TRUE),
  ]
  if (!nrow(rows)) return(NA_character_)
  rows$variable[[1L]]
}

select_wid_income_series <- function(metadata) {
  for (basis in names(wid_income_candidates)) {
    variable <- find_share_variable(metadata, wid_income_candidates[[basis]])
    if (!is.na(variable)) {
      return(list(
        variable = variable,
        basis = basis,
        label = paste(
          if (basis == "post_tax") "Post-tax" else "Pre-tax",
          "national income"
        )
      ))
    }
  }
  NULL
}

select_ess_round_years <- function(years) {
  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years) & years >= 2002L]
  if (!length(years)) return(integer())
  rounds <- split(years, floor((years - 2002L) / 2L))
  sort(vapply(rounds, min, integer(1)))
}

# Returns the first WID region code with ESS years covered by every WID concept.
match_country_with_wid <- function(country, country_ess, wid_countries) {
  wid_name <- if (country %in% names(country_aliases)) {
    country_aliases[[country]]
  } else {
    country
  }
  candidates <- wid_countries$alpha2[
    tolower(wid_countries$shortname) == tolower(wid_name)
  ]

  for (code in candidates) {
    if (!file.exists(wid_file(code, "metadata")) ||
        !file.exists(wid_file(code, "data"))) next

    metadata <- read_wid_metadata(code)
    wealth_variable <- find_share_variable(
      metadata, wid_concepts[["wealth"]]
    )
    income_series <- select_wid_income_series(metadata)
    if (is.na(wealth_variable) || is.null(income_series)) next
    variables <- c(wealth = wealth_variable, income = income_series$variable)

    wid <- read_wid_data(code)
    wid <- wid[
      wid$variable %in% variables & wid$age == "992" & wid$pop == "j" &
        wid$percentile %in% decile_codes & !is.na(wid$value),
    ]
    wid$concept <- names(variables)[match(wid$variable, variables)]

    years_by_concept <- lapply(
      names(variables),
      function(concept) unique(wid$year[wid$concept == concept])
    )
    common_years <- sort(Reduce(
      intersect, c(list(unique(country_ess$year)), years_by_concept)
    ))
    if (!length(common_years)) next

    return(list(
      country = country,
      wid_code = code,
      years = common_years,
      change_years = select_ess_round_years(common_years),
      variables = variables,
      income_basis = income_series$basis,
      income_label = income_series$label,
      wid = wid[
        wid$year %in% common_years,
        c("concept", "percentile", "year", "value")
      ]
    ))
  }
  NULL
}

compute_country_year_matching <- function() {
  ess <- read_ess_country_years()
  wid_countries <- read_wid_countries()

  matches <- list()
  for (country in sort(unique(ess$country))) {
    match <- match_country_with_wid(
      country, ess[ess$country == country, ], wid_countries
    )
    if (!is.null(match)) matches[[country]] <- match
  }

  if (!length(matches)) {
    warning("No country-years were found in both ESS and WID for all concepts.")
    return(list(
      ess = ess[0, ],
      matches = list(),
      country_years = data.frame(
        country = character(), wid_code = character(), year = integer()
      )
    ))
  }

  country_years <- do.call(rbind, lapply(matches, function(match) {
    data.frame(
      country = match$country,
      wid_code = match$wid_code,
      year = match$years,
      stringsAsFactors = FALSE
    )
  }))
  rownames(country_years) <- NULL

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
