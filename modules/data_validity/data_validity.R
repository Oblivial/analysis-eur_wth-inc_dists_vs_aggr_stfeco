validity_rules <- data.frame(
  field = c(
    "ess_reliable", "wealth_complete", "wealth_not_extrapolated",
    "income_complete", "income_not_extrapolated"
  ),
  passing_label = c(
    "Satisfaction estimate reliable", "Complete valid wealth deciles", "Wealth not extrapolated",
    "Complete valid income deciles", "Income not extrapolated"
  ),
  exclusion_reason = c(
    "satisfaction estimate unreliable", "wealth deciles incomplete or invalid",
    "wealth extrapolated",
    "income deciles incomplete or invalid", "income extrapolated"
  )
)

# Check satisfaction reliability and both distributions' completeness and provenance.
assess_country_year <- function(match, ess, year) {
  wealth <- match$wid[match$wid$concept == "wealth", , drop = FALSE]
  income <- match$wid[match$wid$concept == "income", , drop = FALSE]
  # Missing reliability is unknown and therefore fails the validity gate.
  c(
    ess_reliable = isTRUE(ess$reliable[match(year, ess$year)]),
    wealth_complete = !is.null(decile_shares_for_year(wealth, year)),
    wealth_not_extrapolated = !is_extrapolated_year(year, match$extrapolation$wealth),
    income_complete = !is.null(decile_shares_for_year(income, year)),
    income_not_extrapolated = !is_extrapolated_year(year, match$extrapolation$income)
  )
}

# Audit every matched country-year and summarize how many pass each validity rule.
assess_data_validity <- function(matching, rules = validity_rules) {
  failures <- stats::setNames(rules$exclusion_reason, rules$field)
  rows <- list()
  for (match in matching$matches) {
    ess <- matching$ess[matching$ess$country == match$country, , drop = FALSE]
    for (year in match$years) {
      checks <- assess_country_year(match, ess, year)
      rows[[length(rows) + 1L]] <- data.frame(
        country = match$country,
        wid_code = match$wid_code,
        year = year,
        as.list(checks),
        valid = all(checks),
        exclusion_reason = paste(failures[names(checks)[!checks]], collapse = "; ")
      )
    }
  }
  observations <- bind_rows(rows)

  labels <- c(stats::setNames(rules$passing_label, rules$field), valid = "Pass all validity checks")
  summary <- data.frame(
    check = unname(labels),
    passing_country_years = vapply(names(labels), function(field) {
      sum(observations[[field]])
    }, integer(1), USE.NAMES = FALSE),
    total_comparable_country_years = nrow(observations)
  )
  list(observations = observations, summary = summary)
}

# Restrict every downstream data component to the same valid country-year sample.
filter_to_valid_comparable_data <- function(matching, validity) {
  observations <- validity$observations
  if (!nrow(observations)) {
    return(list(ess = matching$ess[0, ], matches = list(), country_years = data.frame()))
  }
  valid <- observations[observations$valid, c("country", "wid_code", "year")]
  rownames(valid) <- NULL

  matches <- lapply(matching$matches, function(match) {
    years <- sort(valid$year[valid$country == match$country])
    if (!length(years)) return(NULL)
    match$years <- years
    match$change_years <- select_survey_round_years(years)
    match$wid <- match$wid[match$wid$year %in% years, , drop = FALSE]
    match
  })

  ess_keys <- paste(matching$ess$country, matching$ess$year)
  list(
    ess = matching$ess[ess_keys %in% paste(valid$country, valid$year), , drop = FALSE],
    matches = Filter(Negate(is.null), matches),
    country_years = valid
  )
}

# Cache the validity audit using source-input and matching-code fingerprints.
run_data_validity <- function(matching) {
  cached_result(
    "data_validity",
    input_files = raw_input_files(),
    code_files = code_files_for("matching", "validity"),
    compute = function() assess_data_validity(matching)
  )
}
