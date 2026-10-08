association_share_summary <- function(data, year) {
  result <- c(
    bottom_50_share = NA_real_,
    middle_40_share = NA_real_,
    top_10_share = NA_real_,
    decile_gini = NA_real_
  )
  rows <- data[data$year == year, , drop = FALSE]
  if (anyDuplicated(rows$percentile)) return(result)
  positions <- match(decile_codes, rows$percentile)
  if (anyNA(positions)) return(result)
  shares <- rows$value[positions]
  if (any(!is.finite(shares)) || abs(sum(shares) - 1) > 0.025) {
    return(result)
  }

  cumulative <- c(0, cumsum(shares))
  area <- sum((cumulative[-length(cumulative)] + cumulative[-1L]) / 2) / 10
  result[["bottom_50_share"]] <- sum(shares[1:5])
  result[["middle_40_share"]] <- sum(shares[6:9])
  result[["top_10_share"]] <- shares[[10L]]
  result[["decile_gini"]] <- 1 - 2 * area
  result
}

association_parse_intervals <- function(text) {
  if (is.na(text) || !nzchar(trimws(text))) return(matrix(integer(), ncol = 2))
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(error) NULL
  )
  values <- unlist(parsed, use.names = FALSE)
  if (!length(values) || length(values) %% 2L != 0L) {
    return(matrix(integer(), ncol = 2))
  }
  matrix(as.integer(values), ncol = 2, byrow = TRUE)
}

association_is_extrapolated <- function(year, intervals) {
  if (!nrow(intervals)) return(FALSE)
  any(year >= intervals[, 1L] & year <= intervals[, 2L])
}

build_ess_wid_country_round_table <- function(matching) {
  panel_rows <- list()

  for (country in names(matching$matches)) {
    match <- matching$matches[[country]]
    country_ess <- matching$ess[
      matching$ess$country == country &
        matching$ess$year %in% match$change_years,
      , drop = FALSE
    ]
    metadata <- read_wid_metadata(match$wid_code)
    interval_by_concept <- lapply(names(match$variables), function(concept) {
      variable <- match$variables[[concept]]
      metadata_row <- metadata[metadata$variable == variable, , drop = FALSE]
      association_parse_intervals(metadata_row$extrapolation[[1L]])
    })
    names(interval_by_concept) <- names(match$variables)

    for (i in seq_len(nrow(country_ess))) {
      ess_row <- country_ess[i, , drop = FALSE]
      year <- ess_row$year[[1L]]
      wealth <- association_share_summary(
        match$wid[match$wid$concept == "wealth", , drop = FALSE], year
      )
      income <- association_share_summary(
        match$wid[match$wid$concept == "income", , drop = FALSE], year
      )
      row <- data.frame(
        country = country,
        wid_code = match$wid_code,
        ess_round = floor((year - 2002L) / 2L) + 1L,
        year = year,
        ess_mean = ess_row$mean[[1L]],
        ess_iqr = ess_row$iqr[[1L]],
        ess_n_effective = ess_row$n_effective[[1L]],
        ess_reliable = ess_row$reliable[[1L]],
        wid_income_series = match$income_label,
        wealth_extrapolated = association_is_extrapolated(
          year, interval_by_concept[["wealth"]]
        ),
        income_extrapolated = association_is_extrapolated(
          year, interval_by_concept[["income"]]
        ),
        stringsAsFactors = FALSE
      )
      for (measure in names(wealth)) {
        row[[paste0("wealth_", measure)]] <- wealth[[measure]]
        row[[paste0("income_", measure)]] <- income[[measure]]
      }
      panel_rows[[length(panel_rows) + 1L]] <- row
    }
  }

  if (length(panel_rows)) {
    panel <- do.call(rbind, panel_rows)
    panel <- panel[order(panel$country, panel$year), , drop = FALSE]
    rownames(panel) <- NULL
    panel
  } else {
    data.frame(
      country = character(), wid_code = character(), ess_round = integer(),
      year = integer(), ess_mean = numeric(), ess_iqr = numeric(),
      ess_n_effective = numeric(), ess_reliable = logical(),
      wid_income_series = character(),
      wealth_extrapolated = logical(), income_extrapolated = logical(),
      wealth_bottom_50_share = numeric(), wealth_middle_40_share = numeric(),
      wealth_top_10_share = numeric(), wealth_decile_gini = numeric(),
      income_bottom_50_share = numeric(), income_middle_40_share = numeric(),
      income_top_10_share = numeric(), income_decile_gini = numeric()
    )
  }
}

association_variables <- c(
  ess_mean = "ESS mean",
  ess_iqr = "ESS IQR",
  wealth_decile_gini = "Wealth decile Gini",
  wealth_bottom_50_share = "Wealth bottom 50% share",
  wealth_top_10_share = "Wealth top 10% share",
  income_decile_gini = "Income decile Gini",
  income_bottom_50_share = "Income bottom 50% share",
  income_top_10_share = "Income top 10% share"
)

compute_association_correlations <- function(panel) {
  data <- panel[panel$ess_reliable %in% TRUE, , drop = FALSE]
  values <- data[names(association_variables)]
  correlations <- lapply(c("pearson", "spearman"), function(method) {
    result <- suppressWarnings(stats::cor(
      values, use = "pairwise.complete.obs", method = method
    ))
    dimnames(result) <- list(
      unname(association_variables), unname(association_variables)
    )
    round(result, 2)
  })
  names(correlations) <- c("pearson", "spearman")
  correlations
}

compute_variance_decomposition <- function(panel) {
  reliable <- panel[panel$ess_reliable %in% TRUE, , drop = FALSE]
  variables <- names(association_variables)
  rows <- lapply(variables, function(variable) {
    values <- reliable[[variable]]
    valid <- is.finite(values)
    data <- reliable[valid, c("country", variable), drop = FALSE]
    if (!nrow(data)) {
      return(data.frame(
        variable = association_variables[[variable]], observations = 0L,
        countries = 0L, between_percent = NA_real_, within_percent = NA_real_
      ))
    }

    country_values <- split(data[[variable]], data$country)
    grand_mean <- mean(data[[variable]])
    between_ss <- sum(vapply(country_values, function(x) {
      length(x) * (mean(x) - grand_mean)^2
    }, numeric(1)))
    within_ss <- sum(vapply(country_values, function(x) {
      sum((x - mean(x))^2)
    }, numeric(1)))
    total_ss <- between_ss + within_ss
    data.frame(
      variable = association_variables[[variable]],
      observations = nrow(data),
      countries = length(country_values),
      between_percent = if (total_ss > 0) 100 * between_ss / total_ss else NA_real_,
      within_percent = if (total_ss > 0) 100 * within_ss / total_ss else NA_real_
    )
  })
  do.call(rbind, rows)
}

compute_stfeco_year_patterns <- function(panel) {
  reliable <- panel[
    panel$ess_reliable %in% TRUE & is.finite(panel$ess_mean), , drop = FALSE
  ]
  years <- sort(unique(reliable$year))
  if (!length(years)) return(data.frame())
  do.call(rbind, lapply(years, function(year) {
    values <- reliable$ess_mean[reliable$year == year]
    data.frame(
      year = year,
      countries = length(values),
      mean = mean(values),
      q25 = unname(stats::quantile(values, 0.25)),
      q75 = unname(stats::quantile(values, 0.75))
    )
  }))
}

plot_stfeco_year_patterns <- function(patterns, relative_file) {
  if (!nrow(patterns)) return(FALSE)
  with_png(relative_file, 1500, 850, function() {
    graphics::par(mar = c(7, 5, 4, 2))
    x <- c(patterns$year, rev(patterns$year))
    y <- c(patterns$q25, rev(patterns$q75))
    graphics::plot(
      patterns$year, patterns$mean,
      type = "n",
      xlab = "ESS observation year",
      ylab = "Mean satisfaction with the state of the economy",
      main = "Reliable cross-country ESS mean by year"
    )
    graphics::polygon(x, y, col = grDevices::adjustcolor("#167D8D", 0.2),
                      border = NA)
    graphics::lines(patterns$year, patterns$mean, col = "#167D8D", lwd = 2)
    graphics::points(
      patterns$year, patterns$mean, pch = 21, bg = "#167D8D",
      cex = 0.6 + 0.6 * patterns$countries / max(patterns$countries)
    )
    graphics::axis(1, at = patterns$year)
    graphics::mtext(
      "Point size scales with the number of reliable countries.",
      side = 1, line = 5, cex = 0.75
    )
  })
}

plot_between_country_scatter <- function(panel, relative_file) {
  data <- panel[panel$ess_reliable %in% TRUE, , drop = FALSE]
  with_png(relative_file, 1800, 950, function() {
    graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
    for (concept in c("wealth", "income")) {
      x_name <- paste0(concept, "_decile_gini")
      complete <- data[is.finite(data$ess_mean) & is.finite(data[[x_name]]), , drop = FALSE]
      country_means <- lapply(split(complete, complete$country), function(rows) {
        data.frame(
          country = rows$country[[1L]],
          ess_mean = mean(rows$ess_mean),
          wid_gini = mean(rows[[x_name]])
        )
      })
      means <- if (length(country_means)) do.call(rbind, country_means) else data.frame()
      graphics::plot(
        means$wid_gini, means$ess_mean,
        pch = 21, bg = "#167D8D",
        xlab = paste("Country mean", if (concept == "wealth") "wealth" else "income", "decile Gini"),
        ylab = "Country mean ESS mean",
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

plot_within_country_scatter <- function(panel, relative_file) {
  data <- panel[panel$ess_reliable %in% TRUE, , drop = FALSE]
  with_png(relative_file, 1800, 950, function() {
    graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
    for (concept in c("wealth", "income")) {
      x_name <- paste0(concept, "_decile_gini")
      complete <- data[
        is.finite(data$ess_mean) & is.finite(data[[x_name]]), , drop = FALSE
      ]
      groups <- split(complete, complete$country)
      demeaned <- lapply(groups, function(rows) {
        data.frame(
          ess_mean = rows$ess_mean - mean(rows$ess_mean),
          wid_gini = rows[[x_name]] - mean(rows[[x_name]])
        )
      })
      points <- if (length(demeaned)) do.call(rbind, demeaned) else data.frame()
      graphics::plot(
        points$wid_gini, points$ess_mean,
        pch = 21, bg = grDevices::adjustcolor("#167D8D", 0.6),
        xlab = paste("Within-country", concept, "decile Gini deviation"),
        ylab = "Within-country ESS mean deviation",
        main = paste("Within countries:", concept)
      )
      graphics::abline(h = 0, v = 0, lty = 3, col = "#777777")
    }
  })
}

association_matrix_table <- function(matrix) {
  data.frame(
    measure = rownames(matrix),
    matrix,
    check.names = FALSE,
    row.names = NULL
  )
}

compute_ess_wid_association <- function(matching) {
  panel <- build_ess_wid_country_round_table(matching)
  table_file <- project_path(paths$association_table)
  dir.create(dirname(table_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(panel, table_file, row.names = FALSE, na = "")

  variance <- compute_variance_decomposition(panel)
  correlations <- compute_association_correlations(panel)
  year_patterns <- compute_stfeco_year_patterns(panel)
  plot_files <- c(
    between = file.path(paths$association_figures, "between_country_scatter.png"),
    within = file.path(paths$association_figures, "within_country_scatter.png"),
    year_patterns = file.path(paths$association_figures, "stfeco_year_patterns.png")
  )
  plot_between_country_scatter(panel, plot_files[["between"]])
  plot_within_country_scatter(panel, plot_files[["within"]])
  plot_stfeco_year_patterns(year_patterns, plot_files[["year_patterns"]])

  list(
    panel = panel,
    variance = variance,
    pearson = association_matrix_table(correlations$pearson),
    spearman = association_matrix_table(correlations$spearman),
    year_patterns = year_patterns,
    table_file = paths$association_table,
    plot_files = plot_files
  )
}

run_ess_wid_association <- function(matching) {
  cached_result(
    "ess_wid_association",
    input_files = raw_input_files(),
    code_files = code_files_for("matching", "validity", "association"),
    compute = function() compute_ess_wid_association(matching),
    output_files = function(result) c(
      result$table_file, unname(result$plot_files)
    )
  )
}
