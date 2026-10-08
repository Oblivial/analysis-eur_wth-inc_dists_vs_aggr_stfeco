parse_validity_intervals <- function(text) {
  if (is.na(text) || !nzchar(trimws(text))) return(matrix(integer(), ncol = 2))
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(error) NULL
  )
  values <- unlist(parsed, use.names = FALSE)
  if (!length(values) || length(values) %% 2L != 0L) {
    return(matrix(integer(), ncol = 2))
  }
  matrix(as.integer(values), ncol = 2, byrow = TRUE)
}

is_year_extrapolated <- function(year, intervals) {
  if (!nrow(intervals)) return(FALSE)
  any(year >= intervals[, 1L] & year <= intervals[, 2L])
}

is_complete_wid_decile_distribution <- function(data, year) {
  rows <- data[data$year == year, , drop = FALSE]
  if (anyDuplicated(rows$percentile)) return(FALSE)
  positions <- match(decile_codes, rows$percentile)
  if (anyNA(positions)) return(FALSE)
  shares <- rows$value[positions]
  all(is.finite(shares)) && abs(sum(shares) - 1) <= 0.025
}

assess_data_validity <- function(matching) {
  observations <- list()
  for (country in names(matching$matches)) {
    match <- matching$matches[[country]]
    metadata <- read_wid_metadata(match$wid_code)
    intervals <- lapply(names(match$variables), function(concept) {
      variable <- match$variables[[concept]]
      row <- metadata[metadata$variable == variable, , drop = FALSE]
      parse_validity_intervals(row$extrapolation[[1L]])
    })
    names(intervals) <- names(match$variables)
    country_ess <- matching$ess[
      matching$ess$country == country & matching$ess$year %in% match$years,
      , drop = FALSE
    ]

    for (year in match$years) {
      ess_row <- country_ess[country_ess$year == year, , drop = FALSE]
      ess_reliable <- nrow(ess_row) == 1L && isTRUE(ess_row$reliable[[1L]])
      wealth_data <- match$wid[
        match$wid$concept == "wealth", , drop = FALSE
      ]
      income_data <- match$wid[
        match$wid$concept == "income", , drop = FALSE
      ]
      wealth_complete <- is_complete_wid_decile_distribution(wealth_data, year)
      income_complete <- is_complete_wid_decile_distribution(income_data, year)
      wealth_extrapolated <- is_year_extrapolated(
        year, intervals[["wealth"]]
      )
      income_extrapolated <- is_year_extrapolated(
        year, intervals[["income"]]
      )
      valid <- ess_reliable && wealth_complete && income_complete &&
        !wealth_extrapolated && !income_extrapolated

      reasons <- c(
        if (!ess_reliable) "ESS unreliable",
        if (!wealth_complete) "wealth deciles incomplete or invalid",
        if (wealth_extrapolated) "wealth extrapolated",
        if (!income_complete) "income deciles incomplete or invalid",
        if (income_extrapolated) "income extrapolated"
      )
      observations[[length(observations) + 1L]] <- data.frame(
        country = country,
        wid_code = match$wid_code,
        year = year,
        ess_reliable = ess_reliable,
        wealth_complete = wealth_complete,
        wealth_extrapolated = wealth_extrapolated,
        wealth_valid = wealth_complete && !wealth_extrapolated,
        income_complete = income_complete,
        income_extrapolated = income_extrapolated,
        income_valid = income_complete && !income_extrapolated,
        valid = valid,
        exclusion_reason = if (length(reasons)) paste(reasons, collapse = "; ") else "",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(observations)) {
    observations <- do.call(rbind, observations)
    rownames(observations) <- NULL
  } else {
    observations <- data.frame(
      country = character(), wid_code = character(), year = integer(),
      ess_reliable = logical(), wealth_complete = logical(),
      wealth_extrapolated = logical(), wealth_valid = logical(),
      income_complete = logical(), income_extrapolated = logical(),
      income_valid = logical(), valid = logical(), exclusion_reason = character()
    )
  }

  check_names <- c(
    ess_reliable = "ESS reliable",
    wealth_complete = "Complete valid wealth deciles",
    wealth_extrapolated = "Wealth not extrapolated",
    income_complete = "Complete valid income deciles",
    income_extrapolated = "Income not extrapolated",
    valid = "Pass all validity checks"
  )
  summary <- data.frame(
    check = unname(check_names),
    passing_country_years = vapply(names(check_names), function(field) {
      if (field %in% c("wealth_extrapolated", "income_extrapolated")) {
        sum(!observations[[field]])
      } else {
        sum(observations[[field]])
      }
    }, integer(1)),
    total_comparable_country_years = nrow(observations),
    stringsAsFactors = FALSE
  )
  list(observations = observations, summary = summary)
}

filter_to_valid_comparable_data <- function(matching, validity) {
  valid_rows <- validity$observations[validity$observations$valid, , drop = FALSE]
  matches <- list()

  for (country in names(matching$matches)) {
    match <- matching$matches[[country]]
    years <- sort(valid_rows$year[valid_rows$country == country])
    if (!length(years)) next
    match$years <- years
    match$change_years <- select_ess_round_years(years)
    match$wid <- match$wid[match$wid$year %in% years, , drop = FALSE]
    matches[[country]] <- match
  }

  valid_keys <- valid_rows[, c("country", "wid_code", "year"), drop = FALSE]
  ess_keys <- paste(matching$ess$country, matching$ess$year, sep = "\r")
  valid_ess_keys <- paste(valid_keys$country, valid_keys$year, sep = "\r")
  ess <- matching$ess[ess_keys %in% valid_ess_keys, , drop = FALSE]
  country_years <- valid_keys
  rownames(country_years) <- NULL
  list(ess = ess, matches = matches, country_years = country_years)
}

run_data_validity <- function(matching) {
  cached_result(
    "data_validity",
    input_files = raw_input_files(),
    code_files = code_files_for("matching", "validity"),
    compute = function() assess_data_validity(matching)
  )
}
