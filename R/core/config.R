project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

project_path <- function(...) file.path(project_root, ...)

paths <- list(
  ess_csv = "data/raw/ess_aggregated_stfeco.csv",
  wid_dir = "data/raw/wid_all_data_2002_2024_europe",
  cache_dir = ".cache",
  coverage_figures = "figures/descriptive/coverage",
  timeseries_figures = "figures/descriptive/timeseries",
  wid_change_figures = "figures/descriptive/distribution_change/wid",
  ess_change_figures = "figures/descriptive/distribution_change/ess"
)

core_files <- c(
  "R/core/config.R",
  "R/core/io.R",
  "R/core/cache.R",
  "R/core/plot_utils.R"
)

module_files <- c(
  matching = "modules/country_year_matching/country_year_matching.R",
  coverage = "modules/data_coverage/data_coverage.R",
  timeseries = "modules/country_timeseries/country_timeseries.R",
  wid_change = "modules/wid_distribution_change/wid_distribution_change.R",
  ess_change = "modules/ess_summary_change/ess_summary_change.R"
)

wid_analysis_years <- 2002:2024

# ESS country names that differ from WID short names.
country_aliases <- c(
  "Czechia" = "Czech Republic",
  "Russian Federation" = "Russia"
)

# Names are the concept keys; values are matched against WID variable descriptions.
wid_concepts <- c(
  wealth = "Net personal wealth",
  income = "Pre-tax national income"
)

wid_income_candidates <- c(
  post_tax = "Post-tax national income",
  pre_tax = "Pre-tax national income"
)

decile_codes <- c(
  "p0p10", "p10p20", "p20p30", "p30p40", "p40p50",
  "p50p60", "p60p70", "p70p80", "p80p90", "p90p100"
)

share_groups <- list(
  "Bottom 50%" = decile_codes[1:5],
  "Middle 40%" = decile_codes[6:9],
  "Top 10%" = decile_codes[10]
)

share_group_colors <- c("#167D8D", "#D58B27", "#B94D55")
