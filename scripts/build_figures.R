source("R/load_project.R")

matching <- run_country_year_matching()
data_validity <- run_data_validity(matching)
valid_matching <- filter_to_valid_comparable_data(matching, data_validity)
data_coverage <- run_data_coverage(matching, valid_matching)
wid_distribution_change <- run_wid_distribution_change(valid_matching)
ess_summary_change <- run_ess_summary_change(valid_matching)
ess_wid_association <- run_ess_wid_association(valid_matching)
