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

regime_panel <- data.frame(
  country = c("A", "A", "B", "A", rep("C", 3), rep("D", 3), "E", "E", "Unknown"),
  ess_mean = c(1, 2, 3, 100, 3, 2, 1, 1, 2, 3, 1, 2, 1),
  wealth_decile_gini = c(1, 2, 3, NA, 1, 2, 3, 1, 1, 1, 1, 2, 1),
  wealth_middle_40_share = c(1, NA, 3, NA, 1, 2, 3, 1, 1, 1, 1, 2, Inf)
)
regime_typology <- data.frame(
  Country = c("A", "B", "C", "D", "E"),
  EA_Type = c("Liberal", "Liberal", "Conservative", "Socialist", "Hybrid1")
)
classification_warning <- NULL
regime_panel <- withCallingHandlers(
  attach_welfare_regimes(regime_panel, regime_typology),
  warning = function(condition) {
    classification_warning <<- conditionMessage(condition)
    invokeRestart("muffleWarning")
  }
)
regime_measures <- define_association_measures()
regime_measures$satisfaction <- regime_measures$satisfaction["ess_mean"]
regime_measures$distributions$wealth <- regime_measures$distributions$wealth[
  c("wealth_decile_gini", "wealth_middle_40_share")
]
regime_correlations <- compute_wealth_regime_correlations(regime_panel, regime_measures)
gini_regimes <- regime_correlations[
  regime_correlations$distribution_label == "Wealth decile Gini", ]
liberal_share <- regime_correlations[
  regime_correlations$welfare_regime == "Liberal" &
    regime_correlations$distribution_label == "Wealth middle 40% share", ]
stopifnot(
  grepl("Unknown", classification_warning),
  nrow(regime_panel) == 13L, regime_panel$welfare_regime[[13L]] == "Not classified",
  nrow(regime_correlations) == 2L * length(welfare_regime_levels),
  identical(gini_regimes$welfare_regime, welfare_regime_levels),
  identical(gini_regimes$n, c(3L, 3L, 3L, 2L, rep(0L, 5), 1L)),
  identical(gini_regimes$countries, c(2L, 1L, 1L, 1L, rep(0L, 5), 1L)),
  isTRUE(all.equal(gini_regimes$pearson[1:2], c(1, -1))),
  isTRUE(all.equal(gini_regimes$spearman[1:2], c(1, -1))),
  all(is.na(gini_regimes$pearson[3:nrow(gini_regimes)])),
  gini_regimes$status[[3L]] == "Constant measure",
  all(gini_regimes$status[4:nrow(gini_regimes)] == "Fewer than 3 pairs"),
  liberal_share$n == 2L, is.na(liberal_share$pearson),
  all(compute_wealth_regime_correlations(regime_panel[0, ], regime_measures)$n == 0L)
)
invalid_typology <- rbind(regime_typology, regime_typology[1, ])
stopifnot(inherits(
  try(attach_welfare_regimes(regime_panel, invalid_typology), silent = TRUE),
  "try-error"
))

post_panel <- data.frame(
  country = rep(c("Estonia", "Portugal", "Iceland", "Turkey", "Unknown"), each = 3),
  ess_mean = c(3, 2, 1, rep(1:3, 4)),
  wealth_decile_gini = rep(1:3, 5),
  wealth_middle_40_share = rep(1:3, 5)
)
post_typology <- data.frame(
  Country = c("Estonia", "Portugal", "Iceland", "Turkey"),
  EA_Type = c("Post-Communist", "Southern European", "Social Democratic", "Other/Non-EU")
)
post_panel <- suppressWarnings(attach_welfare_regimes(post_panel, post_typology))
post_correlations <- compute_wealth_regime_correlations(
  post_panel, regime_measures,
  group_field = "post_communist_status", group_levels = post_communist_levels
)
post_gini <- post_correlations[
  post_correlations$distribution_label == "Wealth decile Gini", ]
stopifnot(
  identical(post_gini$post_communist_status, post_communist_levels),
  identical(post_gini$n, c(3L, 9L, 3L)),
  identical(post_gini$countries, c(1L, 3L, 1L)),
  isTRUE(all.equal(post_gini$pearson, c(-1, 1, 1))),
  isTRUE(all.equal(post_gini$spearman, c(-1, 1, 1))),
  all(compute_wealth_regime_correlations(
    post_panel[0, ], regime_measures,
    group_field = "post_communist_status", group_levels = post_communist_levels
  )$n == 0L)
)

focused_correlations <- compute_focused_wealth_correlations(post_panel)
focused_post <- focused_correlations[
  focused_correlations$welfare_regime == "Post-Communist", ]
focused_non_post <- tail(focused_correlations, 1)
stopifnot(
  nrow(focused_correlations) == length(welfare_regime_levels) + 2L,
  identical(focused_correlations$welfare_regime, c(
    "All countries", welfare_regime_levels, "All non-Post-Communist regimes"
  )),
  all(focused_correlations$distribution_label == "Wealth middle 40% share"),
  all(focused_correlations$satisfaction_label == "Satisfaction mean"),
  focused_correlations$n[[1L]] == 15L,
  focused_correlations$countries[[1L]] == 5L,
  focused_post$n == 3L, focused_post$pearson == -1,
  focused_post$post_communist_status == "Post-Communist",
  focused_non_post$n == 9L, focused_non_post$countries == 3L,
  focused_non_post$pearson == 1,
  focused_non_post$post_communist_status == "Not Post-Communist",
  sum(focused_correlations$post_communist_status == "Post-Communist") == 1L,
  all(compute_focused_wealth_correlations(post_panel[0, ])$n == 0L)
)

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
  cpi2 = c(90, 100, 120, 135, 150),
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
  isTRUE(all.equal(focused_cases$cpi2_change, 15)),
  grepl("increased by 10.00 percentage points", focused_cases$period_interpretation),
  grepl("increased by 2.00 scale points", focused_cases$period_interpretation),
  grepl("CWS CPI2 changed by 15.00 index points", focused_cases$period_interpretation)
)
prepared_cpi2 <- prepare_cws_cpi2(data.frame(
  id = c("FRG", "AUL", "AUL"), year = c("2000", "2000", "2001"),
  cpi2 = c("100", NA, "110")
))
stopifnot(
  identical(prepared_cpi2$country, c("Germany", "Australia")),
  identical(prepared_cpi2$year, c(2000L, 2001L)),
  identical(prepared_cpi2$cpi2, c(100, 110))
)
stopifnot(inherits(
  try(prepare_cws_cpi2(data.frame(id = "XYZ", year = 2000, cpi2 = 1)), silent = TRUE),
  "try-error"
))
cpi2_matching <- list(
  ess = data.frame(country = "Germany", year = c(2002L, 2004L), mean = c(3, 5)),
  matches = list(list(
    country = "Germany", years = c(2002L, 2004L),
    wid = data.frame(
      concept = "wealth", year = rep(c(2002L, 2004L), each = length(decile_codes)),
      percentile = rep(decile_codes, 2), value = rep(0.1, 2 * length(decile_codes))
    )
  ))
)
annual_cpi2 <- annual_middle_wealth_panel(
  cpi2_matching,
  data.frame(country = "Germany", year = c(2002L, 2004L), cpi2 = c(100, 120))
)
stopifnot(identical(annual_cpi2$cpi2, c(100, 120)))
local({
  plot_file <- paste0(".cpi2_change_test_", Sys.getpid(), ".png")
  on.exit(unlink(project_path(plot_file)), add = TRUE)
  plotted <- plot_cpi2_satisfaction_changes(data.frame(
    country = c("A", "B"), period_start = c(2002L, 2004L),
    period_end = c(2004L, 2006L), cpi2_change = c(10, 15),
    satisfaction_change_points = c(1, -1)
  ), plot_file)
  stopifnot(plotted, file.exists(project_path(plot_file)))
})
regime_cases <- data.frame(
  country = c("Ireland", "Denmark", "Estonia", "Austria", "Canada", "Finland"),
  period_start = rep(2002L, 6), period_end = rep(2006L, 6)
)
typology <- data.frame(
  Country = c("Canada", "Austria", "Denmark", "Finland", "Ireland"),
  EA_Type = c("Liberal", "Conservative", "Socialist", "Hybrid1", "Hybrid2")
)
regime_cases <- suppressWarnings(
  sort_middle_wealth_cases_by_regime(regime_cases, typology)
)
stopifnot(
  identical(regime_cases$country,
            c("Canada", "Austria", "Denmark", "Finland", "Ireland", "Estonia")),
  identical(regime_cases$welfare_regime,
            c("Liberal", "Conservative", "Socialist", "Hybrid1", "Hybrid2",
              "Not classified"))
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
