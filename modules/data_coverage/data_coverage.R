summarise_coverage_by_country <- function(country_years) {
  if (!nrow(country_years)) {
    return(data.frame(
      country = character(), wid_code = character(), first_year = integer(),
      last_year = integer(), matched_years = integer()
    ))
  }
  by_country <- do.call(rbind, lapply(
    split(country_years, country_years$country),
    function(rows) {
      data.frame(
        country = rows$country[[1L]],
        wid_code = rows$wid_code[[1L]],
        first_year = min(rows$year),
        last_year = max(rows$year),
        matched_years = nrow(rows),
        stringsAsFactors = FALSE
      )
    }
  ))
  by_country <- by_country[
    order(-by_country$matched_years, by_country$country), , drop = FALSE
  ]
  rownames(by_country) <- NULL
  by_country
}

summarise_coverage_by_year <- function(country_years) {
  if (!nrow(country_years)) {
    return(data.frame(
      year = integer(), country_count = integer(), common_countries = character()
    ))
  }
  countries_by_year <- split(country_years$country, country_years$year)
  by_year <- data.frame(
    year = as.integer(names(countries_by_year)),
    country_count = vapply(
      countries_by_year, function(x) length(unique(x)), integer(1)
    ),
    common_countries = vapply(
      countries_by_year,
      function(x) paste(sort(unique(x)), collapse = ", "),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  by_year <- by_year[order(by_year$year), , drop = FALSE]
  rownames(by_year) <- NULL
  by_year
}

plot_countries_per_year <- function(by_year, relative_file) {
  counts <- by_year$country_count
  with_png(relative_file, 1400, 800, function() {
    graphics::par(mar = c(6, 5, 4, 2))
    positions <- graphics::barplot(
      counts,
      names.arg = by_year$year,
      col = "#167D8D",
      border = NA,
      las = 2,
      ylim = c(0, max(counts) * 1.15),
      xlab = "Year",
      ylab = "Countries with matched ESS and WID data",
      main = "Common-country coverage by year"
    )
    graphics::text(positions, counts, labels = counts, pos = 3, cex = 0.8)
  })
}

plot_years_per_country <- function(by_country, relative_file) {
  by_country <- by_country[order(by_country$matched_years), , drop = FALSE]
  height <- max(900, 36 * nrow(by_country))
  with_png(relative_file, 1500, height, function() {
    graphics::par(mar = c(5, 10, 4, 3))
    positions <- graphics::barplot(
      by_country$matched_years,
      names.arg = by_country$country,
      horiz = TRUE,
      las = 1,
      col = "#D58B27",
      border = NA,
      xlim = c(0, max(by_country$matched_years) * 1.15),
      xlab = "Number of years with matched ESS and WID data",
      main = "Common-year coverage by country",
      cex.names = 0.8
    )
    graphics::text(
      by_country$matched_years, positions,
      labels = by_country$matched_years, pos = 4, cex = 0.75
    )
  })
}

compute_data_coverage <- function(matching) {
  by_country <- summarise_coverage_by_country(matching$country_years)
  by_year <- summarise_coverage_by_year(matching$country_years)

  plot_files <- character()
  if (nrow(by_country)) {
    plot_files <- c(
      countries_per_year = file.path(
        paths$coverage_figures, "common_countries_by_year.png"
      ),
      years_per_country = file.path(
        paths$coverage_figures, "common_years_by_country.png"
      )
    )
    plot_countries_per_year(by_year, plot_files[["countries_per_year"]])
    plot_years_per_country(by_country, plot_files[["years_per_country"]])
  }

  list(by_country = by_country, by_year = by_year, plot_files = plot_files)
}

run_data_coverage <- function(matching) {
  cached_result(
    "data_coverage",
    input_files = raw_input_files(),
    code_files = code_files_for("matching", "coverage"),
    compute = function() compute_data_coverage(matching),
    output_files = function(result) unname(result$plot_files)
  )
}
