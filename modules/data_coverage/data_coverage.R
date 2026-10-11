# Format valid, matched, and excluded observation counts for chart annotations.
coverage_labels <- function(valid, matched) {
  paste0(valid, " (", matched, "/", matched - valid, ")")
}

# Count matched and valid years for each country, retaining its observed date range.
summarise_coverage_by_country <- function(matched, valid) {
  if (!nrow(matched)) return(data.frame())
  by_country <- bind_rows(lapply(split(matched, matched$country), function(rows) {
    country <- rows$country[[1L]]
    data.frame(
      country = country,
      wid_code = rows$wid_code[[1L]],
      first_year = min(rows$year),
      last_year = max(rows$year),
      valid_years = sum(valid$country == country),
      matched_years = nrow(rows)
    )
  }))
  by_country[order(-by_country$valid_years, by_country$country), ]
}

# Count country observations available before and after the validity gate by year.
summarise_coverage_by_year <- function(matched, valid) {
  if (!nrow(matched)) return(data.frame())
  years <- sort(unique(matched$year))
  data.frame(
    year = years,
    valid_count = vapply(years, function(year) sum(valid$year == year), integer(1)),
    matched_count = vapply(years, function(year) sum(matched$year == year), integer(1))
  )
}

# Plot annual valid-country coverage with matched and excluded counts.
plot_countries_per_year <- function(by_year, relative_file) {
  counts <- by_year$valid_count
  with_png(relative_file, 2200, 1050, function() {
    # Extra bottom margin keeps the rotated year labels clear of the x-axis.
    graphics::par(mar = c(6, 6, 5, 2))
    positions <- graphics::barplot(
      counts,
      names.arg = by_year$year,
      col = plot_colors[["teal"]],
      border = NA,
      las = 2,
      ylim = c(0, max(1, counts) * 1.3),
      xlab = "Year",
      ylab = "Valid countries with satisfaction, income and wealth data",
      main = "Valid common-country coverage by year"
    )
    graphics::text(
      positions, counts, pos = 3, cex = 0.72, xpd = NA,
      labels = coverage_labels(counts, by_year$matched_count)
    )
    graphics::mtext(
      "Bar labels: valid (matched / excluded)", side = 3, line = 0.2, cex = 0.75
    )
  })
}

# Plot valid-year coverage for each country with matched and excluded counts.
plot_years_per_country <- function(by_country, relative_file) {
  by_country <- by_country[order(by_country$valid_years), ]
  counts <- by_country$valid_years
  with_png(relative_file, 1500, max(900, 36 * nrow(by_country)), function() {
    # Wide left/right margins reserve space for country names and coverage labels.
    graphics::par(mar = c(5, 10, 5, 7))
    positions <- graphics::barplot(
      counts,
      names.arg = by_country$country,
      horiz = TRUE,
      las = 1,
      col = plot_colors[["orange"]],
      border = NA,
      xlim = c(0, max(1, counts) * 1.35),
      xlab = "Valid years with satisfaction, income and wealth data",
      main = "Valid common-year coverage by country",
      cex.names = 0.8
    )
    graphics::text(
      counts, positions, pos = 4, cex = 0.72, xpd = NA,
      labels = coverage_labels(counts, by_country$matched_years)
    )
    graphics::mtext(
      "Bar labels: valid (matched / excluded)", side = 3, line = 0.2, cex = 0.75
    )
  })
}

# Build coverage summaries and figures before and after validity filtering.
compute_data_coverage <- function(matching, valid_matching) {
  matched <- matching$country_years
  valid <- valid_matching$country_years
  by_country <- summarise_coverage_by_country(matched, valid)
  by_year <- summarise_coverage_by_year(matched, valid)

  plot_files <- character()
  if (nrow(by_country)) {
    plot_files <- c(
      countries_per_year = file.path(paths$coverage_figures, "common_countries_by_year.png"),
      years_per_country = file.path(paths$coverage_figures, "common_years_by_country.png")
    )
    plot_countries_per_year(by_year, plot_files[["countries_per_year"]])
    plot_years_per_country(by_country, plot_files[["years_per_country"]])
  }
  list(by_country = by_country, by_year = by_year, plot_files = plot_files)
}

# Cache coverage outputs using the matching and validity dependencies.
run_data_coverage <- function(matching, valid_matching) {
  cached_result(
    "data_coverage",
    input_files = raw_input_files(),
    code_files = code_files_for("matching", "validity", "coverage"),
    compute = function() compute_data_coverage(matching, valid_matching),
    output_files = function(result) unname(result$plot_files)
  )
}
