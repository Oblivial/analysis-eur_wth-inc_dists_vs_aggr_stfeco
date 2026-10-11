# Combine non-null data frames into a table with fresh row names.
bind_rows <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Assign years to two-year survey rounds counted from the configured anchor.
survey_round <- function(year) (year - survey_round_anchor) %/% 2L + 1L

# Select the earliest available observation in each two-year survey round.
select_survey_round_years <- function(years) {
  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years) & years >= survey_round_anchor]
  if (!length(years)) return(integer())
  as.integer(tapply(years, survey_round(years), min))
}

# WID metadata stores extrapolation ranges as a JSON list of [start, end] pairs.
parse_extrapolation_intervals <- function(text) {
  empty <- matrix(integer(), ncol = 2)
  if (!length(text) || is.na(text) || !nzchar(trimws(text))) return(empty)
  values <- tryCatch(
    unlist(jsonlite::fromJSON(text, simplifyVector = FALSE), use.names = FALSE),
    error = function(error) NULL
  )
  if (!length(values) || length(values) %% 2L) return(empty)
  matrix(as.integer(values), ncol = 2, byrow = TRUE)
}

# Extract each selected WID series' metadata-declared extrapolation intervals.
wid_extrapolation_intervals <- function(metadata, variables) {
  lapply(variables, function(variable) {
    parse_extrapolation_intervals(
      metadata$extrapolation[metadata$variable == variable][1L]
    )
  })
}

# Flag years falling inside any inclusive extrapolation interval.
is_extrapolated_year <- function(years, intervals) {
  if (!nrow(intervals)) return(rep(FALSE, length(years)))
  vapply(years, function(year) {
    any(year >= intervals[, 1L] & year <= intervals[, 2L])
  }, logical(1))
}

# Ten decile shares for one year, or NULL when incomplete or not summing to one.
decile_shares_for_year <- function(data, year) {
  rows <- data[data$year == year, , drop = FALSE]
  if (anyDuplicated(rows$percentile)) return(NULL)
  shares <- rows$value[match(decile_codes, rows$percentile)]
  if (!all(is.finite(shares)) || abs(sum(shares) - 1) > decile_share_tolerance) {
    return(NULL)
  }
  shares
}
