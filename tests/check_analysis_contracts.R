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

cat("Analysis definition and presentation contracts passed.\n")
