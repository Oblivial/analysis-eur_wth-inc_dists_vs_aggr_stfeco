# Single entry point shared by the report and the figure build script.
run_analysis <- function() {
  matching <- run_country_year_matching()
  validity <- run_data_validity(matching)
  valid_matching <- filter_to_valid_comparable_data(matching, validity)
  satisfaction_change <- run_satisfaction_change(valid_matching)
  association <- run_satisfaction_association(valid_matching)
  list(
    validity = validity,
    coverage = run_data_coverage(matching, valid_matching),
    association = association,
    middle_wealth = run_middle_wealth_satisfaction(association$panel, valid_matching),
    distribution_change = run_distribution_change(valid_matching, satisfaction_change$changes),
    satisfaction_change = satisfaction_change
  )
}
