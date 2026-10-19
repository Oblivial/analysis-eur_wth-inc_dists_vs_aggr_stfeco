paths <- list(
  ess_csv = "data/raw/ess_aggregated_stfeco.csv",
  cws_xlsx = "data/raw/cws/CWS-data-2020.xlsx",
  esping_typology_csv = "data/raw/esping_typology_simple.csv",
  wid_dir = "data/raw/wid_all_data_2002_2024_europe",
  cache_dir = ".cache",
  coverage_figures = "figures/descriptive/coverage",
  wid_change_figures = "figures/descriptive/distribution_change/wid",
  ess_change_figures = "figures/descriptive/distribution_change/ess",
  association_figures = "figures/descriptive/ess_wid_association",
  middle_wealth_figures = "figures/middle_wealth_satisfaction",
  association_table = "data/processed/ess_wid_country_round.csv"
)

core_files <- c(
  "modules/config.R",
  "modules/util/io.R",
  "modules/util/cache.R",
  "modules/util/helpers.R",
  "modules/util/welfare_regimes.R",
  "modules/util/plot_utils.R",
  "modules/pipeline.R"
)

module_files <- c(
  matching = "modules/country_year_matching/country_year_matching.R",
  validity = "modules/data_validity/data_validity.R",
  coverage = "modules/data_coverage/data_coverage.R",
  distribution_change = "modules/distribution_change/distribution_change.R",
  satisfaction_change = "modules/satisfaction_change/satisfaction_change.R",
  association = "modules/satisfaction_association/satisfaction_association.R",
  middle_wealth = "modules/middle_wealth_satisfaction/middle_wealth_satisfaction.R"
)

# ESS rounds are two-year windows counted from this year.
survey_round_anchor <- 2002L

# Distribution transitions lead satisfaction transitions by this many survey rounds.
change_lag_rounds <- 3L

# ESS country names that differ from WID short names.
country_aliases <- c(
  "Czechia" = "Czech Republic",
  "Russian Federation" = "Russia"
)

# Matched against WID variable descriptions; income prefers the first available.
wid_wealth_concept <- "Net personal wealth"
wid_income_candidates <- c(
  post_tax = "Post-tax national income",
  pre_tax = "Pre-tax national income"
)

decile_codes <- c(
  "p0p10", "p10p20", "p20p30", "p30p40", "p40p50",
  "p50p60", "p60p70", "p70p80", "p80p90", "p90p100"
)

# Decile shares must sum to one within this tolerance to count as complete.
decile_share_tolerance <- 0.025

plot_colors <- c(
  teal = "#167D8D", red = "#B94D55", orange = "#D58B27", grey = "#777777"
)
