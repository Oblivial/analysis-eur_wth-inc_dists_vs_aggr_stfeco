middle_wealth_settings <- list(
  minimum_rounds = 5L,
  minimum_span_years = 8L,
  strong_spearman = 0.6,
  cases_per_criterion = 2L,
  minimum_lag_pairs = 3L,
  cases_per_sign = 5L,
  excluded_years = 2008L,
  exclude_countries = "Greece",
  focus_intervals = data.frame(
    country = c("Sweden", "Portugal", "Lithuania"),
    start_year = c(2008L, 2015L, 2011L),
    end_year = c(2010L, 2017L, 2013L)
  )
)

# Restrict country cases to finite, reliable, non-extrapolated matched observations.
middle_wealth_panel <- function(panel) {
  keep <- panel$ess_reliable %in% TRUE & panel$wealth_extrapolated %in% FALSE &
    panel$income_extrapolated %in% FALSE & is.finite(panel$ess_mean) &
    is.finite(panel$wealth_middle_40_share)
  data <- panel[keep, c("country", "year", "ess_round", "ess_mean", "wealth_middle_40_share")]
  data[order(data$country, data$year), , drop = FALSE]
}

# Screen within-country associations, coverage, and annualized satisfaction movements.
screen_middle_wealth_cases <- function(panel, settings = middle_wealth_settings) {
  rows <- lapply(split(panel, panel$country), function(data) {
    if (nrow(data) < 2L) return(NULL)
    data <- data[order(data$year), ]
    wealth <- data$wealth_middle_40_share
    satisfaction <- data$ess_mean
    variable_pair <- length(unique(wealth)) > 1L && length(unique(satisfaction)) > 1L
    rank_correlation <- NA_real_
    if (variable_pair) rank_correlation <- stats::cor(wealth, satisfaction, method = "spearman")
    changes <- diff(satisfaction) / diff(data$year)
    largest <- which.max(abs(changes))
    span <- max(data$year) - min(data$year)
    data.frame(
      country = data$country[[1L]], rounds = nrow(data),
      first_year = min(data$year), last_year = max(data$year), span_years = span,
      missing_rounds = max(data$ess_round) - min(data$ess_round) + 1L - nrow(data),
      pearson = if (variable_pair) stats::cor(wealth, satisfaction) else NA_real_,
      spearman = rank_correlation,
      max_mean_change_per_year = abs(changes[[largest]]),
      change_start = data$year[[largest]], change_end = data$year[[largest + 1L]],
      eligible = nrow(data) >= settings$minimum_rounds && span >= settings$minimum_span_years
    )
  })
  bind_rows(rows)
}

# Select complementary cases and retain every reason when a country meets several criteria.
select_middle_wealth_cases <- function(screening, settings = middle_wealth_settings) {
  if (!nrow(screening)) return(screening)
  eligible <- screening[
    screening$eligible & !screening$country %in% settings$exclude_countries, , drop = FALSE
  ]
  strong <- eligible[
    is.finite(eligible$spearman) & abs(eligible$spearman) >= settings$strong_spearman,
    , drop = FALSE
  ]
  criteria <- list(
    "Strong within-country association" = strong[order(-abs(strong$spearman)), ],
    "Long reliable coverage" = eligible[
      order(-eligible$rounds, -eligible$span_years, eligible$missing_rounds, eligible$country),
    ],
    "Large satisfaction change" = eligible[order(-eligible$max_mean_change_per_year), ]
  )
  reasons <- list()
  for (criterion in names(criteria)) {
    for (country in head(criteria[[criterion]]$country, settings$cases_per_criterion)) {
      reasons[[country]] <- c(reasons[[country]], criterion)
    }
  }
  for (index in seq_len(nrow(settings$focus_intervals))) {
    interval <- settings$focus_intervals[index, ]
    if (interval$country %in% screening$country) {
      reason <- sprintf("Requested interval %d-%d", interval$start_year, interval$end_year)
      reasons[[interval$country]] <- c(reasons[[interval$country]], reason)
    }
  }
  if (!length(reasons)) {
    empty <- screening[0, , drop = FALSE]
    empty$selection_reason <- character()
    empty$period_start <- integer()
    empty$period_end <- integer()
    empty$period_source <- character()
    return(empty)
  }
  selected <- screening[match(names(reasons), screening$country), , drop = FALSE]
  selected$selection_reason <- vapply(reasons, paste, character(1), collapse = "; ")
  selected$period_start <- selected$change_start
  selected$period_end <- selected$change_end
  selected$period_source <- "Largest satisfaction-change interval"
  for (index in seq_len(nrow(settings$focus_intervals))) {
    interval <- settings$focus_intervals[index, ]
    row <- match(interval$country, selected$country)
    if (!is.na(row)) {
      selected$period_start[[row]] <- interval$start_year
      selected$period_end[[row]] <- interval$end_year
      selected$period_source[[row]] <- "Requested interval"
    }
  }
  rownames(selected) <- NULL
  selected
}

# Build annual wealth-share and satisfaction observations from the valid matched sample.
annual_middle_wealth_panel <- function(valid_matching) {
  rows <- lapply(valid_matching$matches, function(match) {
    ess <- valid_matching$ess[
      valid_matching$ess$country == match$country, , drop = FALSE
    ]
    wealth <- match$wid[match$wid$concept == "wealth", , drop = FALSE]
    years <- intersect(ess$year, match$years)
    if (!length(years)) return(NULL)
    shares <- lapply(years, function(year) decile_shares_for_year(wealth, year))
    usable <- !vapply(shares, is.null, logical(1))
    years <- years[usable]
    shares <- shares[usable]
    ess <- ess[match(years, ess$year), , drop = FALSE]
    data.frame(
      country = match$country,
      year = years,
      ess_mean = ess$mean,
      wealth_middle_40_share = vapply(shares, function(value) sum(value[6:9]), numeric(1))
    )
  })
  bind_rows(rows)
}

# Match same-year values and exact t-1 wealth values to year-t satisfaction values.
middle_wealth_lag_pairs <- function(annual, country = NULL) {
  if (!is.null(country)) annual <- annual[annual$country == country, , drop = FALSE]
  satisfaction <- annual[c("country", "year", "ess_mean", "wealth_middle_40_share")]
  previous_wealth <- annual[c("country", "year", "wealth_middle_40_share")]
  names(previous_wealth) <- c("country", "wealth_year", "wealth_lagged")
  previous_wealth$year <- previous_wealth$wealth_year + 1L
  merge(satisfaction, previous_wealth, by = c("country", "year"))
}

# Evaluate complete annual periods rather than searching arbitrary windows for high correlations.
screen_supported_wealth_periods <- function(annual, settings = middle_wealth_settings) {
  annual <- annual[!annual$year %in% settings$excluded_years, , drop = FALSE]
  results <- list()
  for (country in sort(unique(annual$country))) {
    if (country %in% settings$exclude_countries) next
    data <- annual[annual$country == country, , drop = FALSE]
    data <- data[order(data$year), ]
    runs <- split(data, cumsum(c(TRUE, diff(data$year) != 1L)))
    for (data in runs) {
      pairs <- middle_wealth_lag_pairs(data)
      if (nrow(pairs) < settings$minimum_lag_pairs) next
      same <- middle_wealth_pair_stats(pairs$wealth_middle_40_share, pairs$ess_mean)
      lag <- middle_wealth_pair_stats(pairs$wealth_lagged, pairs$ess_mean)
      if (!is.finite(same[["pearson"]]) || !is.finite(lag[["pearson"]])) next
      changes <- diff(data$ess_mean)
      largest <- which.max(abs(changes))
      results[[length(results) + 1L]] <- data.frame(
        case_id = paste(country, min(data$year), max(data$year), sep = "_"),
        country = country, rounds = nrow(data), first_year = min(data$year),
        last_year = max(data$year), period_start = min(data$year), period_end = max(data$year),
        period_source = "Complete consecutive valid annual period", eligible = TRUE,
        missing_rounds = 0L, pearson = same[["pearson"]], spearman = same[["spearman"]],
        same_year_n = same[["n"]], same_year_pearson = same[["pearson"]],
        same_year_spearman = same[["spearman"]], lag_n = lag[["n"]],
        lag_pearson = lag[["pearson"]], lag_spearman = lag[["spearman"]],
        max_mean_change_per_year = abs(changes[[largest]]),
        change_start = data$year[[largest]], change_end = data$year[[largest + 1L]]
      )
    }
  }
  bind_rows(results)
}

# Rank Pearson strength with one period per country and sign; explain lag persistence separately.
select_ranked_wealth_periods <- function(screening, settings = middle_wealth_settings) {
  if (!nrow(screening)) return(screening)
  positive <- screening[screening$pearson > 0, , drop = FALSE]
  negative <- screening[screening$pearson < 0, , drop = FALSE]
  positive <- positive[order(-positive$pearson, -positive$lag_n, positive$country), ]
  negative <- negative[order(negative$pearson, -negative$lag_n, negative$country), ]
  positive <- head(positive[!duplicated(positive$country), ], settings$cases_per_sign)
  negative <- head(negative[!duplicated(negative$country), ], settings$cases_per_sign)
  positive$selection_reason <- rep("Strongest positive association", nrow(positive))
  negative$selection_reason <- rep("Strongest negative association", nrow(negative))
  cases <- bind_rows(list(positive, negative))
  cases$lag_interpretation <- vapply(seq_len(nrow(cases)), function(index) {
    same <- cases$same_year_pearson[[index]]
    lag <- cases$lag_pearson[[index]]
    if (sign(same) != sign(lag)) {
      meaning <- "The association reverses direction with the one-year lag."
    } else if (abs(lag) < abs(same)) {
      meaning <- "The association retains its direction but is weaker with the one-year lag."
    } else {
      meaning <- "The association retains its direction and is at least as strong with the lag."
    }
    sprintf("Same-year Pearson %.2f; lagged Pearson %.2f. %s Only %d paired years: exploratory.",
            same, lag, meaning, cases$lag_n[[index]])
  }, character(1))
  cases
}

# Return correlation and sample size for one wealth/satisfaction pairing.
middle_wealth_pair_stats <- function(wealth, satisfaction) {
  keep <- is.finite(wealth) & is.finite(satisfaction)
  wealth <- wealth[keep]
  satisfaction <- satisfaction[keep]
  variable_pair <- length(wealth) >= 2L && length(unique(wealth)) > 1L &&
    length(unique(satisfaction)) > 1L
  pearson <- NA_real_
  spearman <- NA_real_
  if (variable_pair) {
    pearson <- stats::cor(wealth, satisfaction)
    spearman <- stats::cor(wealth, satisfaction, method = "spearman")
  }
  c(
    n = length(wealth),
    pearson = pearson,
    spearman = spearman
  )
}

# Add exact annual same-year and one-year-lag comparisons to each selected case.
add_middle_wealth_lag_stats <- function(cases, annual) {
  if (!nrow(cases)) return(cases)
  cases$same_year_n <- cases$same_year_pearson <- cases$same_year_spearman <- NA_real_
  cases$lag_n <- cases$lag_pearson <- cases$lag_spearman <- NA_real_
  for (index in seq_len(nrow(cases))) {
    data <- annual[annual$country == cases$country[[index]], , drop = FALSE]
    same <- middle_wealth_pair_stats(data$wealth_middle_40_share, data$ess_mean)
    lagged <- middle_wealth_lag_pairs(data)
    lag <- middle_wealth_pair_stats(lagged$wealth_lagged, lagged$ess_mean)
    cases[index, c("same_year_n", "same_year_pearson", "same_year_spearman")] <-
      as.list(unname(same))
    cases[index, c("lag_n", "lag_pearson", "lag_spearman")] <- as.list(unname(lag))
  }
  cases
}

# Attach endpoint changes for each displayed interval in their original units.
add_middle_wealth_period_changes <- function(cases, panel) {
  if (!nrow(cases)) return(cases)
  cases$wealth_share_change_pp <- NA_real_
  cases$satisfaction_change_points <- NA_real_
  for (index in seq_len(nrow(cases))) {
    data <- panel[panel$country == cases$country[[index]], , drop = FALSE]
    start <- data[data$year == cases$period_start[[index]], , drop = FALSE]
    end <- data[data$year == cases$period_end[[index]], , drop = FALSE]
    if (nrow(start) != 1L || nrow(end) != 1L) next
    cases$wealth_share_change_pp[[index]] <- 100 * (
      end$wealth_middle_40_share - start$wealth_middle_40_share
    )
    cases$satisfaction_change_points[[index]] <- end$ess_mean - start$ess_mean
  }
  direction <- function(change) {
    if (change > 0) return("increased")
    if (change < 0) return("decreased")
    "was unchanged"
  }
  cases$period_interpretation <- vapply(seq_len(nrow(cases)), function(index) {
    wealth_change <- cases$wealth_share_change_pp[[index]]
    satisfaction_change <- cases$satisfaction_change_points[[index]]
    if (!is.finite(wealth_change) || !is.finite(satisfaction_change)) {
      return("Requested interval endpoints are not both available in the valid sample.")
    }
    sprintf(
      paste(
        "From %d to %d, the middle-40%% wealth share %s by %.2f percentage points,",
        "while mean economic satisfaction %s by %.2f scale points."
      ),
      cases$period_start[[index]], cases$period_end[[index]],
      direction(wealth_change), abs(wealth_change),
      direction(satisfaction_change), abs(satisfaction_change)
    )
  }, character(1))
  cases
}

# Plot signed country correlations and coverage for cases meeting the minimum sample rules.
plot_middle_wealth_screening <- function(screening, relative_file, settings) {
  data <- screening[screening$eligible & is.finite(screening$pearson), , drop = FALSE]
  if (!nrow(data)) return(FALSE)
  data <- data[order(data$pearson), ]
  labels <- paste0(data$country, " ", data$period_start, "-", data$period_end,
                   " (n=", data$lag_n, ")")
  with_png(relative_file, 1600, max(1000, 55 * nrow(data)), function() {
    graphics::par(mar = c(5, 17, 4, 3))
    positions <- graphics::barplot(
      data$pearson, names.arg = labels,
      horiz = TRUE, las = 1, border = NA, xlim = c(-1, 1), cex.names = 0.85,
      col = ifelse(data$pearson < 0, plot_colors[["red"]], plot_colors[["teal"]]),
      main = "Middle-40% wealth and economic satisfaction: country screening",
      xlab = "Within-period Pearson correlation (shared paired years)"
    )
    graphics::abline(v = 0, col = plot_colors[["grey"]])
    label_positions <- ifelse(data$pearson < 0, 2, 4)
    near_edge <- abs(data$pearson) > 0.95
    label_positions[near_edge] <- ifelse(data$pearson[near_edge] < 0, 4, 2)
    graphics::text(
      data$pearson, positions, labels = sprintf("%.2f", data$pearson),
      pos = label_positions, cex = 0.8
    )
  })
}

# Draw a country's association and aligned trajectories; do not connect missing survey rounds.
plot_middle_wealth_case <- function(case, annual, relative_file) {
  annual <- annual[
    annual$country == case$country & annual$year >= case$period_start &
      annual$year <= case$period_end, , drop = FALSE
  ]
  annual <- annual[order(annual$year), ]
  lagged <- middle_wealth_lag_pairs(annual)
  same_wealth <- 100 * lagged$wealth_middle_40_share
  same_satisfaction <- lagged$ess_mean
  lagged_wealth <- 100 * lagged$wealth_lagged
  lagged_satisfaction <- lagged$ess_mean
  wealth <- 100 * annual$wealth_middle_40_share
  satisfaction <- annual$ess_mean
  with_png(relative_file, 1500, 1500, function() {
    # Three stacked panels share a country title; time series retain their own physical units.
    graphics::par(mfrow = c(3, 1), mar = c(4, 5, 3, 2), oma = c(0, 0, 3, 0))
    graphics::plot(
      same_wealth, same_satisfaction, pch = 21, bg = plot_colors[["teal"]],
      xlim = range(c(same_wealth, lagged_wealth)),
      xlab = "Middle-40% share of net personal wealth (%)",
      ylab = "Economic satisfaction mean (0-10)",
      main = sprintf("Same year: r = %.2f (n = %d); 1-year lag: r = %s (n = %d)",
                     case$same_year_pearson, case$same_year_n,
                     if (is.finite(case$lag_pearson)) sprintf("%.2f", case$lag_pearson) else "NA",
                     case$lag_n)
    )
    graphics::points(lagged_wealth, lagged_satisfaction, pch = 24,
                     bg = plot_colors[["orange"]], col = plot_colors[["red"]])
    if (length(unique(same_wealth)) > 1L) {
      graphics::abline(stats::lm(same_satisfaction ~ same_wealth),
                       col = plot_colors[["teal"]], lwd = 2)
    }
    if (length(unique(lagged_wealth)) > 1L) {
      graphics::abline(stats::lm(lagged_satisfaction ~ lagged_wealth),
                       col = plot_colors[["red"]], lwd = 2, lty = 2)
    }
    graphics::legend(
      "topleft", legend = c("Same year", "Wealth share t-1; satisfaction t"),
      pch = c(21, 24), pt.bg = c(plot_colors[["teal"]], plot_colors[["orange"]]),
      col = c(plot_colors[["teal"]], plot_colors[["red"]]), bty = "n", cex = 0.8
    )
    series <- list(
      list(values = wealth, label = "Middle-40% wealth share (%)", color = plot_colors[["teal"]]),
      list(values = satisfaction, label = "Economic satisfaction mean (0-10)",
           color = plot_colors[["red"]])
    )
    adjacent <- which(diff(annual$year) == 1L)
    for (measure in series) {
      graphics::plot(annual$year, measure$values, pch = 21, bg = measure$color,
                     xlim = range(annual$year), xaxt = "n", xlab = "Survey observation year",
                     ylab = measure$label)
      graphics::axis(1, at = annual$year)
      graphics::segments(annual$year[adjacent], measure$values[adjacent],
                         annual$year[adjacent + 1L], measure$values[adjacent + 1L],
                         col = measure$color, lwd = 2)
      graphics::abline(v = c(case$period_start, case$period_end), lty = 3,
                       col = plot_colors[["grey"]])
    }
    title <- paste(case$country, case$period_start, "-", case$period_end)
    graphics::mtext(title, side = 3, outer = TRUE, line = 1, font = 2)
  })
}

# Build the focused screening and case figures from the existing valid country-round panel.
compute_middle_wealth_satisfaction <- function(
    panel, valid_matching, settings = middle_wealth_settings) {
  annual <- annual_middle_wealth_panel(valid_matching)
  screening <- screen_supported_wealth_periods(annual, settings)
  cases <- add_middle_wealth_period_changes(
    select_ranked_wealth_periods(screening, settings), annual
  )
  plot_files <- character()
  if (nrow(cases)) {
    file <- file.path(paths$middle_wealth_figures, "country_screening.png")
    plot_middle_wealth_screening(cases, file, settings)
    plot_files[["screening"]] <- file
  }
  case_files <- character()
  for (case_index in seq_len(nrow(cases))) {
    case <- cases[case_index, ]
    file <- file.path(paths$middle_wealth_figures, sprintf("case_%02d.png", case_index))
    plot_middle_wealth_case(case, annual, file)
    case_files[[case$case_id]] <- file
  }
  list(settings = settings, screening = cases, candidates = screening, cases = cases,
       plot_files = plot_files, case_files = case_files)
}

# Cache focused case results with their source data, selection criteria, and upstream dependencies.
run_middle_wealth_satisfaction <- function(panel, valid_matching) {
  cached_result(
    "middle_wealth_satisfaction", input_files = raw_input_files(),
    code_files = code_files_for("matching", "validity", "association", "middle_wealth"),
    compute = function() compute_middle_wealth_satisfaction(panel, valid_matching),
    output_files = function(result) c(unname(result$plot_files), unname(result$case_files))
  )
}
