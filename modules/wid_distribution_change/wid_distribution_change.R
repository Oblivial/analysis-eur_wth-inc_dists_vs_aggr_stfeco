parse_extrapolation_intervals <- function(text) {
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

is_extrapolated_year <- function(year, intervals) {
  if (!nrow(intervals)) return(FALSE)
  any(year >= intervals[, 1L] & year <= intervals[, 2L])
}

complete_decile_distributions <- function(data) {
  by_year <- split(data, data$year)
  distributions <- lapply(by_year, function(rows) {
    if (anyDuplicated(rows$percentile)) return(NULL)
    positions <- match(decile_codes, rows$percentile)
    if (anyNA(positions)) return(NULL)
    shares <- rows$value[positions]
    if (any(!is.finite(shares)) || abs(sum(shares) - 1) > 0.025) {
      return(NULL)
    }
    shares
  })
  distributions <- distributions[!vapply(distributions, is.null, logical(1))]
  if (!length(distributions)) return(NULL)

  years <- as.integer(names(distributions))
  distributions <- do.call(rbind, distributions)
  rownames(distributions) <- as.character(years)
  list(years = years, shares = distributions)
}

compute_wid_change_for_series <- function(data, country, code, concept,
                                          series_label, ess_years,
                                          extrapolation_intervals) {
  series <- data[data$concept == concept &
                   data$year %in% wid_analysis_years, , drop = FALSE]
  distributions <- complete_decile_distributions(series)
  if (is.null(distributions)) return(NULL)

  years <- sort(intersect(distributions$years, ess_years))
  if (length(years) < 2L) return(NULL)
  positions <- match(years, distributions$years)
  shares <- distributions$shares
  rows <- lapply(seq.int(2L, length(years)), function(i) {
    previous_year <- years[[i - 1L]]
    year <- years[[i]]
    previous_cumulative <- cumsum(shares[positions[[i - 1L]], ])[1:9]
    current_cumulative <- cumsum(shares[positions[[i]], ])[1:9]
    total_change <- mean(abs(current_cumulative - previous_cumulative)) * 100
    gap <- year - previous_year
    data.frame(
      country = country,
      wid_code = code,
      concept = concept,
      series_label = series_label,
      previous_year = previous_year,
      year = year,
      round_label = paste0(
        "R", floor((previous_year - 2002L) / 2L) + 1L,
        "-R", floor((year - 2002L) / 2L) + 1L
      ),
      interval_years = gap,
      distribution_change_pp = total_change,
      annualized_change_pp = total_change / gap,
      extrapolated = is_extrapolated_year(previous_year, extrapolation_intervals) |
        is_extrapolated_year(year, extrapolation_intervals),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

compute_wid_distribution_changes <- function(matching) {
  changes <- list()

  for (country in names(matching$matches)) {
    match <- matching$matches[[country]]
    metadata <- read_wid_metadata(match$wid_code)
    for (concept in names(match$variables)) {
      variable <- match$variables[[concept]]
      metadata_row <- metadata[metadata$variable == variable, , drop = FALSE]
      extrapolation <- parse_extrapolation_intervals(
        metadata_row$extrapolation[[1L]]
      )
      series_label <- if (concept == "income") {
        match$income_label
      } else {
        "Net personal wealth"
      }
      result <- compute_wid_change_for_series(
        match$wid, country, match$wid_code, concept, series_label,
        match$change_years, extrapolation
      )
      if (!is.null(result)) {
        changes[[length(changes) + 1L]] <- result
      }
    }
  }

  if (length(changes)) {
    changes <- do.call(rbind, changes)
    rownames(changes) <- NULL
  } else {
    changes <- data.frame(
      country = character(), wid_code = character(), concept = character(),
      series_label = character(),
      previous_year = integer(), year = integer(), round_label = character(),
      interval_years = integer(),
      distribution_change_pp = numeric(), annualized_change_pp = numeric(),
      extrapolated = logical()
    )
  }
  changes
}

summarise_wid_change_by_country <- function(changes) {
  if (!nrow(changes)) return(data.frame())
  values <- aggregate(
    annualized_change_pp ~ country + concept + series_label,
    data = changes, median
  )
  names(values)[[4L]] <- "median_annualized_change_pp"
  values
}

plot_wid_change_heatmaps <- function(changes, relative_file) {
  country_count <- length(unique(changes$country))
  with_png(relative_file, 2100, max(2300, 72 * country_count), function() {
    graphics::par(mfrow = c(2, 1), mar = c(8, 11, 4, 7), oma = c(4, 3, 2, 3))
    for (concept in c("wealth", "income")) {
      series_label <- unique(changes$series_label[changes$concept == concept])
      if (!length(series_label)) {
        series_label <- if (concept == "wealth") {
          "Net personal wealth"
        } else {
          "National income"
        }
      }
      draw_change_heatmap_panel(
        changes[changes$concept == concept, , drop = FALSE],
        "annualized_change_pp",
        paste("WID", series_label[[1L]]),
        mark_column = "extrapolated",
        mark_pch = 4,
        marker_note = "x marks a transition with an extrapolated endpoint."
      )
    }
    graphics::mtext(
      "Mean cumulative decile-share movement (percentage points per year)",
      side = 3, outer = TRUE, line = 0.3
    )
    graphics::mtext("Country", side = 2, outer = TRUE, line = 0.7)
  })
}

plot_wid_change_scatter <- function(summary, relative_file) {
  wealth <- summary[summary$concept == "wealth", c(
    "country", "median_annualized_change_pp"
  )]
  income <- summary[summary$concept == "income", c(
    "country", "median_annualized_change_pp"
  )]
  names(wealth)[[2L]] <- "wealth_change"
  names(income)[[2L]] <- "income_change"
  paired <- merge(wealth, income, by = "country")
  if (!nrow(paired)) return(FALSE)

  with_png(relative_file, 1400, 1200, function() {
    graphics::par(mar = c(5, 5, 4, 2))
    graphics::plot(
      paired$wealth_change,
      paired$income_change,
      pch = 21,
      bg = "#167D8D",
      xlab = "Median annualized wealth-distribution change (pp/year)",
      ylab = "Median annualized income-distribution change (pp/year)",
      main = "WID distribution change by country"
    )
    ranks <- rank(-paired$wealth_change) + rank(-paired$income_change)
    labels <- order(ranks)[seq_len(min(10L, nrow(paired)))]
    graphics::text(
      paired$wealth_change[labels], paired$income_change[labels],
      labels = paired$country[labels], pos = 4, cex = 0.7, xpd = NA
    )
    graphics::abline(
      h = stats::median(paired$income_change),
      v = stats::median(paired$wealth_change),
      lty = 3,
      col = "#777777"
    )
  })
}

plot_top_wid_changes <- function(changes, relative_file, n = 15L) {
  if (!nrow(changes)) return(FALSE)
  events <- changes[order(-changes$annualized_change_pp), , drop = FALSE]
  events <- head(events, n)
  events <- events[order(events$annualized_change_pp), , drop = FALSE]
  labels <- paste0(
    events$country, "  ", events$previous_year, "-", events$year,
    "  ", events$series_label,
    ifelse(events$extrapolated, " *", "")
  )
  colors <- ifelse(events$concept == "wealth", "#167D8D", "#B94D55")

  with_png(relative_file, 1800, max(950, 45 * nrow(events)), function() {
    graphics::par(mar = c(5, 19, 4, 3))
    positions <- graphics::barplot(
      events$annualized_change_pp,
      names.arg = labels,
      horiz = TRUE,
      las = 1,
      col = colors,
      border = NA,
      xlim = c(0, max(events$annualized_change_pp) * 1.18),
      xlab = "Annualized distribution change (pp/year)",
      main = paste("Largest", nrow(events), "WID distribution changes"),
      cex.names = 0.75
    )
    graphics::text(
      events$annualized_change_pp, positions,
      labels = round(events$annualized_change_pp, 2), pos = 4, cex = 0.75
    )
    graphics::legend(
      "bottomright", legend = c("Wealth", "Income"),
      fill = c("#167D8D", "#B94D55"), bty = "n"
    )
  })
}

compute_wid_distribution_change <- function(matching) {
  changes <- compute_wid_distribution_changes(matching)
  summary <- summarise_wid_change_by_country(changes)
  plot_files <- character()

  if (nrow(changes)) {
    plot_files <- c(
      heatmaps = file.path(paths$wid_change_figures, "change_heatmaps.png"),
      scatter = file.path(paths$wid_change_figures, "country_comparison.png"),
      top_changes = file.path(paths$wid_change_figures, "largest_changes.png")
    )
    plot_wid_change_heatmaps(changes, plot_files[["heatmaps"]])
    plot_wid_change_scatter(summary, plot_files[["scatter"]])
    plot_top_wid_changes(changes, plot_files[["top_changes"]])
  }
  list(changes = changes, country_summary = summary, plot_files = plot_files)
}

run_wid_distribution_change <- function(matching) {
  cached_result(
    "wid_distribution_change",
    input_files = c(ess_input_files(), wid_input_files()),
    code_files = code_files_for("matching", "wid_change"),
    compute = function() compute_wid_distribution_change(matching),
    output_files = function(result) unname(result$plot_files)
  )
}
