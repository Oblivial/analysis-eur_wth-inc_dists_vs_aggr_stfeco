# Compute annualized satisfaction magnitudes between consecutive selected survey observations.
compute_satisfaction_transitions <- function(matching) {
  rows <- lapply(split(matching$ess, matching$ess$country), function(data) {
    data <- data[is.finite(data$mean), , drop = FALSE]
    # Compare one earliest available observation per round, not every ESS year.
    data <- data[data$year %in% select_survey_round_years(data$year), , drop = FALSE]
    data <- data[order(data$year), , drop = FALSE]
    if (nrow(data) < 2L) return(NULL)

    previous <- data[-nrow(data), ]
    current <- data[-1L, ]
    gap <- current$year - previous$year
    data.frame(
      country = current$country,
      previous_year = previous$year,
      year = current$year,
      interval_years = gap,
      mean_change_per_year = abs(current$mean - previous$mean) / gap,
      iqr_change_per_year = abs(current$iqr - previous$iqr) / gap,
      previous_iqr = previous$iqr,
      current_iqr = current$iqr,
      reliable_pair = previous$reliable & current$reliable
    )
  })
  bind_rows(rows)
}

# Select the largest finite changes, then order them for horizontal plotting.
largest_changes <- function(changes, column, n) {
  changes <- changes[is.finite(changes[[column]]), , drop = FALSE]
  top <- head(changes[order(-changes[[column]]), , drop = FALSE], n)
  top[order(top[[column]]), , drop = FALSE]
}

# Label country-year endpoint pairs and mark transitions with unreliable endpoints.
transition_labels <- function(changes) {
  paste0(
    changes$country, "  ", changes$previous_year, "-", changes$year,
    ifelse(changes$reliable_pair, "", " *")
  )
}

# Display the largest satisfaction mean changes and before/after IQR values.
plot_top_satisfaction_changes <- function(changes, relative_file, n = 15L) {
  if (!nrow(changes)) return(FALSE)
  mean_panel <- largest_changes(changes, "mean_change_per_year", n)
  iqr_panel <- largest_changes(changes, "iqr_change_per_year", n)

  with_png(relative_file, 1900, 1800, function() {
    # Stack the two summaries; the outer bottom margin holds the shared note.
    graphics::par(mfrow = c(2, 1), mar = c(4, 19, 3, 3), oma = c(2, 0, 1, 0))
    if (nrow(mean_panel)) {
      values <- mean_panel$mean_change_per_year
      positions <- graphics::barplot(
        values,
        names.arg = transition_labels(mean_panel),
        horiz = TRUE,
        las = 1,
        col = plot_colors[["teal"]],
        border = NA,
        xlim = c(0, max(max(values) * 1.18, 0.1)),
        xlab = "Annualized absolute satisfaction change (points/year)",
        main = "Largest satisfaction mean changes",
        cex.names = 0.75
      )
      graphics::text(values, positions, labels = round(values, 2), pos = 4, cex = 0.75)
    } else {
      empty_panel("Largest mean changes")
    }

    if (nrow(iqr_panel)) {
      y <- seq_len(nrow(iqr_panel))
      limits <- range(c(iqr_panel$previous_iqr, iqr_panel$current_iqr))
      if (diff(limits) == 0) limits <- limits + c(-0.5, 0.5)
      widened <- iqr_panel$current_iqr >= iqr_panel$previous_iqr
      colors <- ifelse(widened, plot_colors[["red"]], plot_colors[["teal"]])
      graphics::plot(
        NA,
        xlim = limits,
        ylim = c(0.5, length(y) + 0.5),
        yaxt = "n",
        xlab = "Satisfaction IQR (scale points)",
        ylab = "",
        main = "Largest satisfaction IQR changes: before and after each survey round",
        yaxs = "i"
      )
      graphics::axis(2, at = y, labels = transition_labels(iqr_panel), las = 1, cex.axis = 0.75)
      graphics::segments(
        iqr_panel$previous_iqr, y, iqr_panel$current_iqr, y, col = colors, lwd = 2
      )
      graphics::points(
        iqr_panel$previous_iqr, y, pch = 21, bg = plot_colors[["grey"]], col = "#444444"
      )
      graphics::points(iqr_panel$current_iqr, y, pch = 21, bg = colors, col = colors)
    } else {
      empty_panel("Largest IQR changes")
    }
    graphics::mtext(
      paste(
        "* Unreliable/unknown endpoint. Grey = previous; colored = current.",
        "Red = wider IQR; teal = narrower."
      ),
      side = 1, outer = TRUE, line = 0.5, cex = 0.8
    )
  })
}

# Build satisfaction transitions and their largest-change comparison figure.
compute_satisfaction_change <- function(matching) {
  changes <- compute_satisfaction_transitions(matching)
  plot_files <- character()
  if (nrow(changes)) {
    plot_files <- c(
      top_changes = file.path(paths$ess_change_figures, "largest_changes.png")
    )
    plot_top_satisfaction_changes(changes, plot_files[["top_changes"]])
  }
  list(changes = changes, plot_files = plot_files)
}

# Cache satisfaction-change outputs using the matching and validity dependencies.
run_satisfaction_change <- function(matching) {
  cached_result(
    "ess_summary_change",
    input_files = raw_input_files(),
    code_files = code_files_for("matching", "validity", "satisfaction_change"),
    compute = function() compute_satisfaction_change(matching),
    output_files = function(result) unname(result$plot_files)
  )
}
