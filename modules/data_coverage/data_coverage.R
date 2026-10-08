summarise_coverage_by_country <- function(country_years, ess) {
  if (!nrow(country_years)) {
    return(data.frame(
      country = character(), wid_code = character(), first_year = integer(),
      last_year = integer(), reliable_years = integer(),
      matched_years = integer(), invalid_years = integer()
    ))
  }
  matched <- merge(
    country_years,
    ess[, c("country", "year", "reliable")],
    by = c("country", "year"),
    all.x = TRUE,
    sort = FALSE
  )
  matched$reliable <- !is.na(matched$reliable) & matched$reliable
  by_country <- do.call(rbind, lapply(
    split(matched, matched$country),
    function(rows) {
      data.frame(
        country = rows$country[[1L]],
        wid_code = rows$wid_code[[1L]],
        first_year = min(rows$year),
        last_year = max(rows$year),
        reliable_years = sum(rows$reliable),
        matched_years = nrow(rows),
        invalid_years = sum(!rows$reliable),
        stringsAsFactors = FALSE
      )
    }
  ))
  by_country <- by_country[
    order(-by_country$reliable_years, by_country$country), , drop = FALSE
  ]
  rownames(by_country) <- NULL
  by_country
}

summarise_coverage_by_year <- function(country_years, ess) {
  if (!nrow(country_years)) {
    return(data.frame(
      year = integer(), country_count = integer(), matched_count = integer(),
      invalid_count = integer(), common_countries = character()
    ))
  }
  matched <- merge(
    country_years,
    ess[, c("country", "year", "reliable")],
    by = c("country", "year"),
    all.x = TRUE,
    sort = FALSE
  )
  matched$reliable <- !is.na(matched$reliable) & matched$reliable
  rows_by_year <- split(matched, matched$year)
  by_year <- data.frame(
    year = as.integer(names(rows_by_year)),
    country_count = vapply(rows_by_year, function(rows) {
      sum(rows$reliable)
    }, integer(1)),
    matched_count = vapply(rows_by_year, nrow, integer(1)),
    invalid_count = vapply(rows_by_year, function(rows) {
      sum(!rows$reliable)
    }, integer(1)),
    common_countries = vapply(
      rows_by_year,
      function(rows) paste(sort(unique(rows$country[rows$reliable])),
                           collapse = ", "),
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
  labels <- paste0(
    by_year$country_count, " (", by_year$matched_count, "/",
    by_year$invalid_count, ")"
  )
  with_png(relative_file, 2200, 1050, function() {
    graphics::par(mar = c(6, 6, 5, 2))
    positions <- graphics::barplot(
      counts,
      names.arg = by_year$year,
      col = "#167D8D",
      border = NA,
      las = 2,
      ylim = c(0, max(1, max(counts)) * 1.3),
      xlab = "Year",
      ylab = "Countries with reliable ESS and matched WID data",
      main = "Reliable common-country coverage by year"
    )
    graphics::text(
      positions, counts, labels = labels, pos = 3, cex = 0.72, xpd = NA
    )
    graphics::mtext(
      "Bar labels: reliable (matched / invalid)", side = 3, line = 0.2,
      cex = 0.75
    )
  })
}

plot_years_per_country <- function(by_country, relative_file) {
  by_country <- by_country[order(by_country$reliable_years), , drop = FALSE]
  counts <- by_country$reliable_years
  labels <- paste0(
    by_country$reliable_years, " (", by_country$matched_years, "/",
    by_country$invalid_years, ")"
  )
  height <- max(900, 36 * nrow(by_country))
  with_png(relative_file, 1500, height, function() {
    graphics::par(mar = c(5, 10, 5, 7))
    positions <- graphics::barplot(
      counts,
      names.arg = by_country$country,
      horiz = TRUE,
      las = 1,
      col = "#D58B27",
      border = NA,
      xlim = c(0, max(1, max(counts)) * 1.35),
      xlab = "Reliable years with matched ESS and WID data",
      main = "Reliable common-year coverage by country",
      cex.names = 0.8
    )
    graphics::text(
      counts, positions, labels = labels, pos = 4, cex = 0.72, xpd = NA
    )
    graphics::mtext(
      "Bar labels: reliable (matched / invalid)", side = 3, line = 0.2,
      cex = 0.75
    )
  })
}

compute_data_coverage <- function(matching) {
  by_country <- summarise_coverage_by_country(
    matching$country_years, matching$ess
  )
  by_year <- summarise_coverage_by_year(matching$country_years, matching$ess)

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
