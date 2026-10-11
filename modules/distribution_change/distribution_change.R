# Mean absolute shift across the nine cumulative decile cutoffs, in pp per year.
compute_distribution_transition <- function(match, concept) {
  data <- match$wid[match$wid$concept == concept, , drop = FALSE]
  shares <- lapply(match$change_years, function(year) decile_shares_for_year(data, year))
  names(shares) <- match$change_years
  shares <- Filter(Negate(is.null), shares)
  if (length(shares) < 2L) return(NULL)

  years <- as.integer(names(shares))
  cumulative <- vapply(shares, function(share) cumsum(share)[1:9], numeric(9))
  # Cumulative shares keep this distance meaningful for negative net-wealth deciles.
  previous_year <- years[-length(years)]
  year <- years[-1L]
  gap <- year - previous_year
  total_change <- 100 * colMeans(abs(
    cumulative[, -1L, drop = FALSE] - cumulative[, -ncol(cumulative), drop = FALSE]
  ))
  intervals <- match$extrapolation[[concept]]

  data.frame(
    country = match$country,
    wid_code = match$wid_code,
    concept = concept,
    series_label = if (concept == "income") match$income_label else wid_wealth_concept,
    previous_year = previous_year,
    year = year,
    interval_years = gap,
    distribution_change_pp = total_change,
    annualized_change_pp = total_change / gap,
    extrapolated = is_extrapolated_year(previous_year, intervals) |
      is_extrapolated_year(year, intervals)
  )
}

# Collect income and wealth transition magnitudes for every matched country.
compute_distribution_transitions <- function(matching) {
  rows <- list()
  for (match in matching$matches) {
    for (concept in names(match$variables)) {
      rows[[length(rows) + 1L]] <- compute_distribution_transition(match, concept)
    }
  }
  bind_rows(rows)
}

# Summarize each country's typical transition magnitude with the median.
median_by_country <- function(values, countries) {
  tapply(values, countries, stats::median)
}

# Retain finite satisfaction changes with reliable observations at both endpoints.
reliable_satisfaction_changes <- function(satisfaction_changes) {
  if (!nrow(satisfaction_changes)) return(satisfaction_changes)
  satisfaction_changes[
    satisfaction_changes$reliable_pair & is.finite(satisfaction_changes$mean_change_per_year), ,
    drop = FALSE
  ]
}

# Country medians of wealth, income and satisfaction change for countries with all three.
country_change_summary <- function(changes, satisfaction_changes) {
  ess <- reliable_satisfaction_changes(satisfaction_changes)
  if (!nrow(ess)) return(data.frame())
  wealth <- changes[changes$concept == "wealth", ]
  income <- changes[changes$concept == "income", ]
  wealth <- median_by_country(wealth$annualized_change_pp, wealth$country)
  income <- median_by_country(income$annualized_change_pp, income$country)
  ess_mean <- median_by_country(ess$mean_change_per_year, ess$country)
  countries <- Reduce(intersect, list(names(wealth), names(income), names(ess_mean)))
  data.frame(
    country = countries,
    wealth_change = as.numeric(wealth[countries]),
    income_change = as.numeric(income[countries]),
    satisfaction_mean_change = as.numeric(ess_mean[countries])
  )
}

# Plot typical wealth versus income movement, sizing points by satisfaction movement.
plot_distribution_change_scatter <- function(paired, relative_file) {
  if (!nrow(paired)) return(FALSE)
  satisfaction_limits <- range(paired$satisfaction_mean_change, finite = TRUE)
  with_png(relative_file, 1400, 1200, function() {
    # Leave room for axis labels and country annotations around the scatterplot.
    graphics::par(mar = c(5, 5, 4, 2))
    graphics::plot(
      paired$wealth_change,
      paired$income_change,
      pch = 21,
      bg = plot_colors[["teal"]],
      cex = scale_point_size(paired$satisfaction_mean_change, satisfaction_limits),
      xlab = "Median annualized wealth-distribution change (pp/year)",
      ylab = "Median annualized income-distribution change (pp/year)",
      main = "Income and wealth distribution change by country"
    )
    # Combined ranks prioritize countries among the largest changes on either axis.
    ranks <- rank(-paired$wealth_change) + rank(-paired$income_change)
    labels <- order(ranks)[seq_len(min(10L, nrow(paired)))]
    graphics::text(
      paired$wealth_change[labels], paired$income_change[labels],
      labels = paired$country[labels], pos = 4, cex = 0.7, xpd = NA
    )
    graphics::abline(
      h = stats::median(paired$income_change),
      v = stats::median(paired$wealth_change),
      lty = 3, col = plot_colors[["grey"]]
    )
    size_breaks <- unique(round(
      stats::quantile(paired$satisfaction_mean_change, c(0.1, 0.5, 0.9), names = FALSE), 2
    ))
    graphics::legend(
      "topleft",
      legend = paste(size_breaks, "pp/year"),
      pch = 21,
      pt.bg = plot_colors[["teal"]],
      pt.cex = scale_point_size(size_breaks, satisfaction_limits),
      title = "Satisfaction mean change",
      bty = "n",
      cex = 0.8
    )
  })
}

# Rank the largest income and wealth transitions in a horizontal bar chart.
plot_top_distribution_changes <- function(changes, relative_file, n = 15L) {
  if (!nrow(changes)) return(FALSE)
  events <- head(changes[order(-changes$annualized_change_pp), ], n)
  events <- events[order(events$annualized_change_pp), ]
  values <- events$annualized_change_pp
  labels <- paste0(
    events$country, "  ", events$previous_year, "-", events$year,
    "  ", events$series_label, ifelse(events$extrapolated, " *", "")
  )
  colors <- ifelse(events$concept == "wealth", plot_colors[["teal"]], plot_colors[["red"]])

  with_png(relative_file, 1800, max(950, 45 * nrow(events)), function() {
    graphics::par(mar = c(5, 19, 4, 3))
    positions <- graphics::barplot(
      values,
      names.arg = labels,
      horiz = TRUE,
      las = 1,
      col = colors,
      border = NA,
      xlim = c(0, max(values) * 1.18),
      xlab = "Annualized distribution change (pp/year)",
      main = paste("Largest", nrow(events), "income and wealth distribution changes"),
      cex.names = 0.75
    )
    graphics::text(values, positions, labels = round(values, 2), pos = 4, cex = 0.75)
    graphics::legend(
      "bottomright", legend = c("Wealth", "Income"),
      fill = plot_colors[c("teal", "red")], bty = "n"
    )
  })
}

# Describe the comparisons once; calculations, tables, and plots share these definitions.
define_change_comparisons <- function(lag_rounds = change_lag_rounds) {
  if (length(lag_rounds) != 1L || !is.finite(lag_rounds) ||
        lag_rounds < 1L || lag_rounds != as.integer(lag_rounds)) {
    stop("lag_rounds must be a positive integer.")
  }
  data.frame(
    comparison_id = c("same_interval", "lagged"),
    lag_rounds = c(0L, lag_rounds),
    alignment = c("Same interval", sprintf("Distribution %d rounds earlier", lag_rounds)),
    plot_label = c("same interval", sprintf("%d-round lag", lag_rounds)),
    description = c(
      "Same-interval comparisons match identical endpoint years.",
      sprintf(paste(
        "Lagged comparisons match both distribution endpoints %d survey rounds earlier",
        "than the satisfaction endpoints; missing rounds are not skipped."
      ), lag_rounds)
    )
  )
}

# Match transition endpoints; comparison definitions carry their labels independently of data.
pair_satisfaction_distribution_changes <- function(
    satisfaction_changes, distribution_changes, comparisons = define_change_comparisons()) {
  ess <- reliable_satisfaction_changes(satisfaction_changes)
  if (!nrow(ess) || !nrow(distribution_changes)) return(data.frame())
  ess <- data.frame(
    country = ess$country, satisfaction_start = ess$previous_year, satisfaction_end = ess$year,
    satisfaction_change = ess$mean_change_per_year
  )
  wid <- distribution_changes[
    !distribution_changes$extrapolated & is.finite(distribution_changes$annualized_change_pp),
  ]
  wid <- data.frame(
    country = wid$country, distribution_start = wid$previous_year, distribution_end = wid$year,
    concept = wid$concept, distribution_change = wid$annualized_change_pp
  )

  same <- merge(
    ess, wid,
    by.x = c("country", "satisfaction_start", "satisfaction_end"),
    by.y = c("country", "distribution_start", "distribution_end")
  )
  same$distribution_start <- same$satisfaction_start
  same$distribution_end <- same$satisfaction_end
  same$comparison_id <- rep("same_interval", nrow(same))

  # Shift both round endpoints; missing rounds are not treated as adjacent observations.
  lag_rounds <- comparisons$lag_rounds[comparisons$comparison_id == "lagged"]
  ess$start_round <- survey_round(ess$satisfaction_start) - lag_rounds
  ess$end_round <- survey_round(ess$satisfaction_end) - lag_rounds
  wid$start_round <- survey_round(wid$distribution_start)
  wid$end_round <- survey_round(wid$distribution_end)
  lagged <- merge(
    ess, wid, by = c("country", "start_round", "end_round")
  )
  lagged$comparison_id <- rep("lagged", nrow(lagged))

  columns <- c(
    "country", "concept", "comparison_id", "satisfaction_start", "satisfaction_end",
    "distribution_start", "distribution_end",
    "satisfaction_change", "distribution_change"
  )
  rbind(same[columns], lagged[columns])
}

# Evaluate every requested comparison, retaining empty samples as n = 0 and NA correlations.
summarise_change_association <- function(paired, comparisons = define_change_comparisons()) {
  results <- list()
  for (comparison_index in seq_len(nrow(comparisons))) {
    comparison <- comparisons[comparison_index, ]
    for (concept in c("income", "wealth")) {
      data <- paired[
        paired$concept == concept & paired$comparison_id == comparison$comparison_id,
        , drop = FALSE
      ]
      correlate <- function(method) {
        constant_satisfaction <- length(unique(data$satisfaction_change)) < 2L
        if (constant_satisfaction || length(unique(data$distribution_change)) < 2L) {
          return(NA_real_)
        }
        stats::cor(data$distribution_change, data$satisfaction_change, method = method)
      }
      results[[length(results) + 1L]] <- data.frame(
        concept = concept,
        comparison_id = comparison$comparison_id,
        alignment = comparison$alignment,
        plot_label = comparison$plot_label,
        n = nrow(data),
        pearson = correlate("pearson"),
        spearman = correlate("spearman")
      )
    }
  }
  bind_rows(results)
}

# Plot satisfaction versus distribution change for each domain and alignment.
plot_change_association <- function(paired, summary, relative_file) {
  if (!nrow(summary)) return(FALSE)
  with_png(relative_file, 1600, 1400, function() {
    # Four panels compare income/wealth by alignment; mar is bottom/left/top/right in text lines.
    graphics::par(mfrow = c(2, 2), mar = c(4.2, 4.2, 3.2, 1))
    for (panel_index in seq_len(nrow(summary))) {
      row <- summary[panel_index, ]
      data <- paired[
        paired$concept == row$concept & paired$comparison_id == row$comparison_id,
        , drop = FALSE
      ]
      title <- sprintf(
        "%s: %s\nn = %d; Spearman = %s",
        tools::toTitleCase(row$concept), row$plot_label, row$n,
        if (is.finite(row$spearman)) sprintf("%.2f", row$spearman) else "NA"
      )
      if (!nrow(data)) {
        empty_panel(title, "No comparable transitions")
        next
      }
      graphics::plot(
        data$distribution_change, data$satisfaction_change,
        pch = 21, bg = plot_colors[["teal"]], col = "#14535B",
        xlab = "Annualized distribution change (pp/year)",
        ylab = "Annualized satisfaction mean change (points/year)",
        main = title
      )
      if (length(unique(data$distribution_change)) > 1L) {
        graphics::abline(
          stats::lm(satisfaction_change ~ distribution_change, data = data),
          col = plot_colors[["red"]], lwd = 1.5
        )
      }
    }
  })
}

# Build distribution transitions, change correlations, and comparison figures.
compute_distribution_change <- function(matching, satisfaction_changes) {
  changes <- compute_distribution_transitions(matching)
  comparisons <- define_change_comparisons()
  paired <- pair_satisfaction_distribution_changes(satisfaction_changes, changes, comparisons)
  change_association <- summarise_change_association(paired, comparisons)
  plot_files <- character()

  if (nrow(changes)) {
    figures <- paths$wid_change_figures
    plot_files <- c(
      scatter = file.path(figures, "country_comparison.png"),
      top_changes = file.path(figures, "largest_changes.png"),
      change_association = file.path(figures, "ess_wid_change_association.png")
    )
    plot_distribution_change_scatter(
      country_change_summary(changes, satisfaction_changes), plot_files[["scatter"]]
    )
    plot_top_distribution_changes(changes, plot_files[["top_changes"]])
    plot_change_association(paired, change_association, plot_files[["change_association"]])
  }
  list(
    changes = changes,
    comparisons = comparisons,
    paired_changes = paired,
    change_association = change_association,
    plot_files = plot_files
  )
}

# Cache distribution-change outputs using matching, validity, and satisfaction dependencies.
run_distribution_change <- function(matching, satisfaction_changes) {
  cached_result(
    "wid_distribution_change",
    input_files = raw_input_files(),
    code_files = code_files_for(
      "matching", "validity", "satisfaction_change", "distribution_change"
    ),
    compute = function() compute_distribution_change(matching, satisfaction_changes),
    output_files = function(result) unname(result$plot_files)
  )
}
