source("modules/load_project.R")

satisfaction <- data.frame(
  country = "A", previous_year = 2008L, year = 2010L,
  mean_change_per_year = 0.4, reliable_pair = TRUE
)
distribution <- data.frame(
  country = "A", previous_year = c(2004L, 2006L, 2008L, 2002L),
  year = c(2006L, 2008L, 2010L, 2006L), concept = "income",
  annualized_change_pp = c(1, 2, 3, 4), extrapolated = FALSE
)

# Changing one definition updates endpoint matching and both presentation labels.
for (lag in c(1L, 2L, 3L)) {
  comparisons <- define_change_comparisons(lag)
  paired <- pair_satisfaction_distribution_changes(satisfaction, distribution, comparisons)
  summary <- summarise_change_association(paired, comparisons)
  stopifnot(
    nrow(summary) == 4L,
    summary$plot_label[[3L]] == paste0(lag, "-round lag"),
    summary$alignment[[3L]] == sprintf("Distribution %d rounds earlier", lag),
    summary$n[[3L]] == as.integer(lag < 3L)
  )
  if (lag < 3L) {
    lagged <- paired[paired$comparison_id == "lagged", ]
    stopifnot(lagged$distribution_start == 2008L - 2L * lag)
  }
}

# Requested comparisons stay visible even when no usable observations remain.
empty <- summarise_change_association(data.frame(), define_change_comparisons(2L))
stopifnot(nrow(empty) == 4L, all(empty$n == 0L), all(is.na(empty$pearson)))

# Measure inclusion propagates to the numerical tables and their report counterparts.
panel <- data.frame(
  ess_mean = c(1, 2, 3), ess_median = c(1, 2, 4), ess_std = c(2, 1, 3),
  ess_p10 = c(0, 1, 2), ess_p90 = c(3, 4, 5), ess_iqr = c(1, 3, 2)
)
for (domain in c("wealth", "income")) {
  for (field in names(distribution_variables(domain))) panel[[field]] <- c(0.1, 0.3, 0.2)
}
selection <- satisfaction_measures
before <- compute_association_correlations(panel, define_association_measures(selection))
selection$correlation[selection$field == "ess_median"] <- TRUE
after <- compute_association_correlations(panel, define_association_measures(selection))
stopifnot(
  nrow(before$pearson_income) == 5L,
  nrow(after$pearson_income) == 6L,
  identical(after$pearson_income, after$report_tables$pearson_income$table)
)

panel$country <- c("A", "A", "B")
panel$income_bottom_50_share[[1L]] <- NA_real_
scatter_pairs <- select_association_scatters(panel)
stopifnot(
  "wealth_middle_40_share" %in% scatter_pairs$distribution,
  !"income_bottom_50_share" %in% scatter_pairs$distribution,
  !anyDuplicated(scatter_pairs$distribution),
  all(scatter_pairs$n == 3L),
  nrow(select_association_scatters(panel[0, ])) == 0L
)
panel$income_bottom_50_share[[1L]] <- 0.1
scatter_pairs <- select_association_scatters(panel)
stopifnot("income_bottom_50_share" %in% scatter_pairs$distribution)

matching <- list(
  ess = data.frame(country = "A", year = 2002L, reliable = FALSE),
  matches = list(list(
    country = "A", wid_code = "AA", years = 2002L,
    wid = data.frame(concept = character(), year = integer(), percentile = character(),
                     value = numeric()),
    extrapolation = list(wealth = matrix(integer(), ncol = 2), income = matrix(integer(), ncol = 2))
  ))
)
normal <- assess_data_validity(matching)
reordered <- assess_data_validity(matching, validity_rules[rev(seq_len(nrow(validity_rules))), ])
stopifnot(identical(normal$observations, reordered$observations))

case_panel <- data.frame(
  country = "A", year = c(2002L, 2004L, 2008L, 2010L, 2012L),
  ess_round = c(1L, 2L, 4L, 5L, 6L), ess_mean = c(1, 2, 3, 5, 6),
  wealth_middle_40_share = c(0.2, 0.3, 0.4, 0.5, 0.6),
  ess_reliable = TRUE, wealth_extrapolated = FALSE, income_extrapolated = FALSE
)
screening <- screen_middle_wealth_cases(middle_wealth_panel(case_panel))
selected <- select_middle_wealth_cases(screening)
stopifnot(
  screening$eligible, screening$missing_rounds == 1L,
  isTRUE(all.equal(screening$spearman, 1)),
  screening$max_mean_change_per_year == 1,
  screening$change_start == 2008L, screening$change_end == 2010L,
  nrow(selected) == 1L,
  grepl("Strong within-country association", selected$selection_reason),
  grepl("Large satisfaction change", selected$selection_reason)
)
focused_settings <- middle_wealth_settings
focused_settings$focus_intervals <- data.frame(
  country = "A", start_year = 2008L, end_year = 2010L
)
focused_cases <- add_middle_wealth_period_changes(
  select_middle_wealth_cases(screening, focused_settings), case_panel
)
stopifnot(
  identical(middle_wealth_settings$focus_intervals$country,
            c("Sweden", "Portugal", "Lithuania")),
  identical(middle_wealth_settings$exclude_countries, "Greece"),
  focused_cases$period_source == "Requested interval",
  isTRUE(all.equal(focused_cases$wealth_share_change_pp, 10)),
  isTRUE(all.equal(focused_cases$satisfaction_change_points, 2)),
  grepl("increased by 10.00 percentage points", focused_cases$period_interpretation),
  grepl("increased by 2.00 scale points", focused_cases$period_interpretation)
)
annual_pairs <- data.frame(
  country = "A", year = c(2008L, 2009L, 2010L),
  ess_mean = c(2, 3, 4), wealth_middle_40_share = c(0.4, 0.5, 0.6)
)
lag_pairs <- middle_wealth_lag_pairs(annual_pairs)
lag_stats <- middle_wealth_pair_stats(lag_pairs$wealth_lagged, lag_pairs$ess_mean)
stopifnot(nrow(lag_pairs) == 2L, all(lag_pairs$year - lag_pairs$wealth_year == 1L),
          lag_stats[["n"]] == 2L)
lithuania_pairs <- middle_wealth_lag_pairs(data.frame(
  country = "Lithuania", year = c(2011L, 2013L),
  ess_mean = c(2, 3), wealth_middle_40_share = c(0.4, 0.5)
))
stopifnot(nrow(lithuania_pairs) == 0L)
case_panel$ess_reliable[[1L]] <- FALSE
screening <- screen_middle_wealth_cases(middle_wealth_panel(case_panel))
stopifnot(!screening$eligible, nrow(select_middle_wealth_cases(screening)) == 0L)
case_panel$ess_reliable <- TRUE
case_panel$wealth_middle_40_share <- 0.3
constant <- screen_middle_wealth_cases(middle_wealth_panel(case_panel))
stopifnot(is.na(constant$spearman), nrow(select_middle_wealth_cases(constant)) == 1L)
case_panel$wealth_extrapolated <- TRUE
stopifnot(nrow(middle_wealth_panel(case_panel)) == 0L)

supported <- data.frame(
  country = "Supported", year = 2002:2007, ess_mean = 1:6,
  wealth_middle_40_share = seq(0.2, 0.7, by = 0.1)
)
sparse <- supported[c(1, 3, 5), ]
sparse$country <- "Sparse"
period_screen <- screen_supported_wealth_periods(rbind(supported, sparse))
ranked_periods <- select_ranked_wealth_periods(period_screen)
stopifnot(
  nrow(period_screen) == 1L, ranked_periods$country == "Supported",
  ranked_periods$lag_n == 5L, ranked_periods$same_year_n == ranked_periods$lag_n,
  ranked_periods$period_start == 2002L, ranked_periods$period_end == 2007L
)

crisis_series <- data.frame(
  country = "Crisis", year = 2003:2012, ess_mean = 1:10,
  wealth_middle_40_share = seq(0.1, 1, by = 0.1)
)
crisis_period <- screen_supported_wealth_periods(crisis_series)
stopifnot(
  nrow(crisis_period) == 2L,
  identical(crisis_period$period_start, c(2003L, 2009L)),
  identical(crisis_period$period_end, c(2007L, 2012L)),
  all(crisis_period$lag_n >= 3L),
  all(crisis_period$period_end < 2008L | crisis_period$period_start > 2008L)
)
period_data <- crisis_series[
  crisis_series$year >= crisis_period$period_start[[1L]] &
    crisis_series$year <= crisis_period$period_end[[1L]],
]
crisis_pairs <- middle_wealth_lag_pairs(period_data)
stopifnot(!any(crisis_pairs$year == 2008L), !any(crisis_pairs$wealth_year == 2008L))

effect_cases <- data.frame(
  country = c("A", "A", "A", "B"), case_id = c("A_early", "A_later", "A_negative", "B"),
  pearson = c(0.4, 0.9, -0.8, 0.6), spearman = c(0.9, 0.5, -0.8, 0.6),
  same_year_pearson = c(0.4, 0.9, -0.8, 0.6),
  lag_pearson = c(0.2, 0.88, -0.85, -0.5), lag_n = c(8L, 3L, 3L, 4L)
)
effect_selected <- select_ranked_wealth_periods(effect_cases)
stopifnot(
  identical(effect_selected$case_id, c("A_later", "B", "A_negative")),
  !"A_early" %in% effect_selected$case_id,
  grepl("retains its direction", effect_selected$lag_interpretation[[1L]]),
  grepl("reverses direction", effect_selected$lag_interpretation[[2L]])
)

cat("Analysis definition and presentation contracts passed.\n")
