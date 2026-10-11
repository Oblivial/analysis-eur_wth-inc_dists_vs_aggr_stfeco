distribution_measure_labels <- c(
  decile_gini = "decile Gini",
  palma_ratio = "Palma ratio",
  middle_40_share = "middle 40% share",
  bottom_50_share = "bottom 50% share",
  top_10_share = "top 10% share"
)

# Map income or wealth measures to labelled table columns.
distribution_variables <- function(concept) {
  stats::setNames(
    paste(tools::toTitleCase(concept), distribution_measure_labels),
    paste0(concept, "_", names(distribution_measure_labels))
  )
}

satisfaction_measures <- data.frame(
  field = c("ess_mean", "ess_median", "ess_std", "ess_p10", "ess_p90", "ess_iqr"),
  label = paste("Satisfaction", c("mean", "median", "standard deviation", "p10", "p90", "IQR")),
  correlation = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
  variance = TRUE
)

# Resolve measure selections once for calculations and report descriptions.
define_association_measures <- function(selection = satisfaction_measures) {
  labels <- stats::setNames(selection$label, selection$field)
  list(
    satisfaction = labels[selection$correlation],
    distributions = lapply(c(wealth = "wealth", income = "income"), distribution_variables),
    variance = c(
      labels[selection$variance], distribution_variables("wealth"), distribution_variables("income")
    ),
    selection = selection[c("label", "correlation", "variance")]
  )
}

# Gini is the decile approximation; Palma is missing when bottom-40 is not positive.
distribution_measures <- function(shares) {
  measures <- c(
    bottom_40_share = NA_real_, bottom_50_share = NA_real_, middle_40_share = NA_real_,
    top_10_share = NA_real_, decile_gini = NA_real_, palma_ratio = NA_real_
  )
  if (is.null(shares)) return(measures)
  cumulative <- c(0, cumsum(shares))
  bottom_40 <- sum(shares[1:4])
  c(
    bottom_40_share = bottom_40,
    bottom_50_share = sum(shares[1:5]),
    middle_40_share = sum(shares[6:9]),
    top_10_share = shares[[10L]],
    decile_gini = 1 - sum(cumulative[-11L] + cumulative[-1L]) / 10,
    palma_ratio = if (bottom_40 > 0) shares[[10L]] / bottom_40 else NA_real_
  )
}

# Join satisfaction summaries and income/wealth measures for selected survey rounds.
build_country_round_table <- function(matching) {
  rows <- lapply(matching$matches, function(match) {
    # The earliest matched year per two-year round keeps each country-round unique.
    ess <- matching$ess[
      matching$ess$country == match$country & matching$ess$year %in% match$change_years, ,
      drop = FALSE
    ]
    if (!nrow(ess)) return(NULL)
    ess <- ess[order(ess$year), ]

    measures <- lapply(c(wealth = "wealth", income = "income"), function(concept) {
      data <- match$wid[match$wid$concept == concept, , drop = FALSE]
      values <- t(vapply(ess$year, function(year) {
        distribution_measures(decile_shares_for_year(data, year))
      }, numeric(6)))
      colnames(values) <- paste0(concept, "_", colnames(values))
      values
    })
    ess_columns <- ess[c("mean", "median", "std", "p10", "p90", "iqr", "n_effective", "reliable")]
    names(ess_columns) <- paste0("ess_", names(ess_columns))

    data.frame(
      country = match$country,
      wid_code = match$wid_code,
      ess_round = survey_round(ess$year),
      year = ess$year,
      ess_columns,
      wid_income_series = match$income_label,
      wealth_extrapolated = is_extrapolated_year(ess$year, match$extrapolation$wealth),
      income_extrapolated = is_extrapolated_year(ess$year, match$extrapolation$income),
      measures$wealth,
      measures$income
    )
  })
  panel <- bind_rows(rows)
  if (!nrow(panel)) return(panel)
  panel <- panel[order(panel$country, panel$year), ]
  rownames(panel) <- NULL
  panel
}

# Compute pairwise-complete correlations and attach readable measure labels.
labelled_correlation <- function(data, rows, columns, method) {
  result <- suppressWarnings(stats::cor(
    data[names(rows)], data[names(columns)],
    use = "pairwise.complete.obs", method = method
  ))
  dimnames(result) <- list(unname(rows), unname(columns))
  data.frame(measure = rownames(result), round(result, 2), check.names = FALSE, row.names = NULL)
}

# Compare satisfaction summaries with each distribution domain separately.
compute_association_correlations <- function(reliable, measures = define_association_measures()) {
  tables <- list()
  report_tables <- list()
  groups <- c(list(satisfaction = measures$satisfaction), measures$distributions)
  for (method in c("pearson", "spearman")) {
    for (group in names(groups)) {
      key <- if (group == "satisfaction") {
        paste(group, method, sep = "_")
      } else {
        paste(method, group, sep = "_")
      }
      tables[[key]] <- labelled_correlation(
        reliable, measures$satisfaction, groups[[group]], method
      )
      report_tables[[key]] <- list(
        title = paste(tools::toTitleCase(method), tools::toTitleCase(group), sep = ": "),
        table = tables[[key]]
      )
    }
  }
  tables$report_tables <- report_tables
  tables
}

# Report between-country and within-country shares of each measure's total variation.
compute_variance_decomposition <- function(reliable, measures = define_association_measures()) {
  # Split total variation into country means and deviations within each country.
  bind_rows(lapply(names(measures$variance), function(variable) {
    values <- reliable[[variable]]
    keep <- is.finite(values)
    values <- values[keep]
    groups <- split(values, reliable$country[keep])
    between <- sum(vapply(groups, function(x) length(x) * (mean(x) - mean(values))^2, numeric(1)))
    within <- sum(vapply(groups, function(x) sum((x - mean(x))^2), numeric(1)))
    total <- between + within
    data.frame(
      variable = measures$variance[[variable]],
      observations = length(values),
      countries = length(groups),
      between_percent = if (total > 0) 100 * between / total else NA_real_,
      within_percent = if (total > 0) 100 * within / total else NA_real_
    )
  }))
}

# Summarize the cross-country satisfaction mean and interquartile range by year.
compute_satisfaction_year_patterns <- function(reliable) {
  reliable <- reliable[is.finite(reliable$ess_mean), , drop = FALSE]
  if (!nrow(reliable)) return(data.frame())
  by_year <- split(reliable$ess_mean, reliable$year)
  data.frame(
    year = as.integer(names(by_year)),
    countries = lengths(by_year),
    mean = vapply(by_year, mean, numeric(1)),
    q25 = vapply(by_year, stats::quantile, numeric(1), 0.25, names = FALSE),
    q75 = vapply(by_year, stats::quantile, numeric(1), 0.75, names = FALSE),
    row.names = NULL
  )
}

# Plot annual satisfaction means with a cross-country interquartile band.
plot_satisfaction_year_patterns <- function(patterns, relative_file) {
  if (!nrow(patterns)) return(FALSE)
  teal <- plot_colors[["teal"]]
  with_png(relative_file, 1500, 850, function() {
    # The larger bottom margin accommodates the survey-year ticks and chart note.
    graphics::par(mar = c(7, 5, 4, 2))
    graphics::plot(
      patterns$year, patterns$mean,
      type = "n",
      xlab = "Survey observation year",
      ylab = "Mean satisfaction with the state of the economy",
      main = "Cross-country economic satisfaction by year"
    )
    graphics::polygon(
      c(patterns$year, rev(patterns$year)), c(patterns$q25, rev(patterns$q75)),
      col = grDevices::adjustcolor(teal, 0.2), border = NA
    )
    graphics::lines(patterns$year, patterns$mean, col = teal, lwd = 2)
    graphics::points(
      patterns$year, patterns$mean, pch = 21, bg = teal,
      cex = 0.6 + 0.6 * patterns$countries / max(patterns$countries)
    )
    graphics::axis(1, at = patterns$year)
    graphics::mtext(
      "Point size scales with the number of reliable countries.", side = 1, line = 5, cex = 0.75
    )
  })
}

# Country means or within-country deviations of satisfaction and income/wealth Gini.
gini_points <- function(reliable, concept, demean) {
  gini <- paste0(concept, "_decile_gini")
  data <- reliable[is.finite(reliable$ess_mean) & is.finite(reliable[[gini]]), , drop = FALSE]
  bind_rows(lapply(split(data, data$country), function(rows) {
    if (demean) {
      data.frame(
        country = rows$country,
        ess_mean = rows$ess_mean - mean(rows$ess_mean),
        wid_gini = rows[[gini]] - mean(rows[[gini]])
      )
    } else {
      data.frame(
        country = rows$country[[1L]],
        ess_mean = mean(rows$ess_mean),
        wid_gini = mean(rows[[gini]])
      )
    }
  }))
}

# Compare country-average satisfaction with country-average income/wealth Gini.
plot_between_country_scatter <- function(reliable, relative_file) {
  with_png(relative_file, 1800, 950, function() {
    # Keep wealth and income side by side with matching panel dimensions.
    graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
    for (concept in c("wealth", "income")) {
      means <- gini_points(reliable, concept, demean = FALSE)
      graphics::plot(
        means$wid_gini, means$ess_mean,
        pch = 21, bg = plot_colors[["teal"]],
        xlab = paste("Country mean", concept, "decile Gini"),
        ylab = "Country-average satisfaction (scale points)",
        main = paste("Between countries:", concept)
      )
      if (nrow(means) > 2L) {
        labels <- unique(c(
          which.min(means$wid_gini), which.max(means$wid_gini),
          which.min(means$ess_mean), which.max(means$ess_mean)
        ))
        graphics::text(
          means$wid_gini[labels], means$ess_mean[labels],
          labels = means$country[labels], pos = 4, cex = 0.65, xpd = NA
        )
      }
    }
  })
}

# Compare satisfaction and Gini deviations from each country's own mean.
plot_within_country_scatter <- function(reliable, relative_file) {
  with_png(relative_file, 1800, 950, function() {
    # Use the same side-by-side layout as the between-country comparison.
    graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
    for (concept in c("wealth", "income")) {
      points <- gini_points(reliable, concept, demean = TRUE)
      graphics::plot(
        points$wid_gini, points$ess_mean,
        pch = 21, bg = grDevices::adjustcolor(plot_colors[["teal"]], 0.6),
        xlab = paste("Within-country", concept, "decile Gini deviation"),
        ylab = "Within-country satisfaction deviation (points)",
        main = paste("Within countries:", concept)
      )
      graphics::abline(h = 0, v = 0, lty = 3, col = plot_colors[["grey"]])
    }
  })
}

# Rank each distribution metric by its strongest satisfaction pairing, retaining requested shares.
select_association_scatters <- function(reliable, measures = define_association_measures(),
                                        per_domain = 3L) {
  required <- c(wealth = "wealth_middle_40_share", income = "income_bottom_50_share")
  selected <- list()
  for (domain in names(measures$distributions)) {
    candidates <- list()
    for (distribution in names(measures$distributions[[domain]])) {
      for (satisfaction in names(measures$satisfaction)) {
        keep <- is.finite(reliable[[distribution]]) & is.finite(reliable[[satisfaction]])
        x <- reliable[[distribution]][keep]
        y <- reliable[[satisfaction]][keep]
        if (length(x) < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) next
        candidates[[length(candidates) + 1L]] <- data.frame(
          domain = domain, distribution = distribution, satisfaction = satisfaction,
          distribution_label = measures$distributions[[domain]][[distribution]],
          satisfaction_label = measures$satisfaction[[satisfaction]],
          n = length(x), countries = length(unique(reliable$country[keep])),
          pearson = stats::cor(x, y), spearman = stats::cor(x, y, method = "spearman")
        )
      }
    }
    ranked <- bind_rows(candidates)
    if (!nrow(ranked)) next
    ranked <- ranked[order(-abs(ranked$spearman), -abs(ranked$pearson)), ]
    ranked <- ranked[!duplicated(ranked$distribution), ]
    chosen <- unique(c(head(ranked$distribution, per_domain), required[[domain]]))
    selected[[domain]] <- ranked[ranked$distribution %in% chosen, ]
  }
  bind_rows(selected)
}

# Build the country-round export, descriptive tables, and association figures.
compute_satisfaction_association <- function(matching) {
  panel <- build_country_round_table(matching)
  table_file <- project_path(paths$association_table)
  dir.create(dirname(table_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(panel, table_file, row.names = FALSE, na = "")

  reliable <- panel[panel$ess_reliable %in% TRUE, , drop = FALSE]
  measures <- define_association_measures()
  selected_associations <- select_association_scatters(reliable, measures)
  year_patterns <- compute_satisfaction_year_patterns(reliable)
  figures <- paths$association_figures
  plot_files <- c(
    between = file.path(figures, "between_country_scatter.png"),
    within = file.path(figures, "within_country_scatter.png"),
    year_patterns = file.path(figures, "stfeco_year_patterns.png")
  )
  plot_between_country_scatter(reliable, plot_files[["between"]])
  plot_within_country_scatter(reliable, plot_files[["within"]])
  plot_satisfaction_year_patterns(year_patterns, plot_files[["year_patterns"]])
  for (domain in unique(selected_associations$domain)) {
    key <- paste0("selected_", domain)
    plot_files[[key]] <- file.path(figures, paste0(key, "_scatter.png"))
    plot_selected_associations(
      reliable, selected_associations[selected_associations$domain == domain, ], plot_files[[key]]
    )
  }

  c(
    list(
      panel = panel, variance = compute_variance_decomposition(reliable, measures),
      measure_selection = measures$selection,
      selected_associations = selected_associations
    ),
    compute_association_correlations(reliable, measures),
    list(
      year_patterns = year_patterns,
      table_file = paths$association_table,
      plot_files = plot_files
    )
  )
}
# Plot complete country-round pairs with percentage share axes and descriptive fitted lines.
plot_selected_associations <- function(reliable, selected, relative_file) {
  if (!nrow(selected)) return(FALSE)
  with_png(relative_file, 1600, 900 * nrow(selected), function() {
    # Stack panels to keep labels legible at the report's constrained content width.
    graphics::par(mfrow = c(nrow(selected), 1), mar = c(6, 5, 4, 1))
    for (panel_index in seq_len(nrow(selected))) {
      relationship <- selected[panel_index, ]
      x <- reliable[[relationship$distribution]]
      y <- reliable[[relationship$satisfaction]]
      keep <- is.finite(x) & is.finite(y)
      x <- x[keep]
      y <- y[keep]
      share <- grepl("_share$", relationship$distribution)
      if (share) x <- 100 * x
      x_label <- paste0(relationship$distribution_label, if (share) " (%)" else "")
      graphics::plot(
        x, y, pch = 21, cex = 0.85,
        bg = grDevices::adjustcolor(plot_colors[["teal"]], 0.45),
        col = grDevices::adjustcolor(plot_colors[["teal"]], 0.7),
        xlab = paste(strwrap(x_label, width = 35), collapse = "\n"),
        ylab = paste(strwrap(paste(relationship$satisfaction_label, "(scale points)"),
                             width = 35), collapse = "\n"),
        main = sprintf("n = %d; countries = %d\nPearson = %.2f; Spearman = %.2f",
                       relationship$n, relationship$countries,
                       relationship$pearson, relationship$spearman)
      )
      graphics::abline(stats::lm(y ~ x), col = plot_colors[["red"]], lwd = 2)
    }
  })
}

# Reuse association outputs when source inputs, analysis code, and files are unchanged.
run_satisfaction_association <- function(matching) {
  cached_result(
    "ess_wid_association",
    input_files = raw_input_files(),
    code_files = code_files_for("matching", "validity", "association"),
    compute = function() compute_satisfaction_association(matching),
    output_files = function(result) c(result$table_file, unname(result$plot_files))
  )
}
