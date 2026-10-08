compute_ess_summary_changes <- function() {
  ess <- read_ess_country_years()
  changes <- list()

  for (country in sort(unique(ess$country))) {
    country_data <- ess[ess$country == country, , drop = FALSE]
    country_data <- country_data[order(country_data$year), , drop = FALSE]
    country_data <- country_data[is.finite(country_data$mean), , drop = FALSE]
    country_data <- country_data[
      country_data$year %in% select_ess_round_years(country_data$year), ,
      drop = FALSE
    ]
    if (nrow(country_data) < 2L) next

    rows <- lapply(seq.int(2L, nrow(country_data)), function(i) {
      previous <- country_data[i - 1L, , drop = FALSE]
      current <- country_data[i, , drop = FALSE]
      gap <- current$year - previous$year
      iqr_change <- if (is.finite(previous$iqr) && is.finite(current$iqr)) {
        abs(current$iqr - previous$iqr) / gap
      } else {
        NA_real_
      }
      data.frame(
        country = country,
        previous_year = previous$year,
        year = current$year,
        round_label = paste0(
          "R", floor((previous$year - 2002L) / 2L) + 1L,
          "-R", floor((current$year - 2002L) / 2L) + 1L
        ),
        interval_years = gap,
        mean_change_per_year = abs(current$mean - previous$mean) / gap,
        iqr_change_per_year = iqr_change,
        previous_iqr = previous$iqr,
        current_iqr = current$iqr,
        reliable_pair = isTRUE(previous$reliable) && isTRUE(current$reliable),
        stringsAsFactors = FALSE
      )
    })
    changes[[country]] <- do.call(rbind, rows)
  }

  if (length(changes)) {
    changes <- do.call(rbind, changes)
    rownames(changes) <- NULL
  } else {
    changes <- data.frame(
      country = character(), previous_year = integer(), year = integer(),
      round_label = character(), interval_years = integer(),
      mean_change_per_year = numeric(),
      iqr_change_per_year = numeric(), reliable_pair = logical()
    )
  }
  changes
}

plot_ess_change_heatmaps <- function(changes, relative_file) {
  if (!nrow(changes)) return(FALSE)
  mean_changes <- data.frame(
    country = changes$country,
    previous_year = changes$previous_year,
    year = changes$year,
    round_label = changes$round_label,
    magnitude = ifelse(
      changes$reliable_pair, changes$mean_change_per_year, NA_real_
    )
  )
  iqr_changes <- data.frame(
    country = changes$country,
    previous_year = changes$previous_year,
    year = changes$year,
    round_label = changes$round_label,
    magnitude = ifelse(
      changes$reliable_pair, changes$iqr_change_per_year, NA_real_
    )
  )
  country_count <- length(unique(changes$country))

  with_png(relative_file, 2100, max(2300, 72 * country_count), function() {
    graphics::par(mfrow = c(2, 1), mar = c(8, 11, 4, 7), oma = c(4, 3, 2, 3))
    draw_change_heatmap_panel(
      mean_changes, "magnitude", "ESS mean change",
      marker_note = "Blank cells have an unreliable or unknown endpoint.",
      scale_label = "ESS points/year"
    )
    draw_change_heatmap_panel(
      iqr_changes, "magnitude", "ESS IQR change",
      marker_note = "Blank cells have an unreliable or unknown endpoint.",
      scale_label = "ESS points/year"
    )
    graphics::mtext(
      "Annualized absolute changes (ESS scale points per year)",
      side = 3, outer = TRUE, line = 0.3
    )
    graphics::mtext("Country", side = 2, outer = TRUE, line = 0.7)
  })
}

plot_top_ess_changes <- function(changes, relative_file, n = 15L) {
  if (!nrow(changes)) return(FALSE)
  mean_rows <- changes[
    is.finite(changes$mean_change_per_year), , drop = FALSE
  ]
  iqr_rows <- changes[
    is.finite(changes$iqr_change_per_year) &
      is.finite(changes$previous_iqr) & is.finite(changes$current_iqr), ,
    drop = FALSE
  ]
  if (!nrow(mean_rows) && !nrow(iqr_rows)) return(FALSE)

  with_png(relative_file, 1900, 1800, function() {
    graphics::par(mfrow = c(2, 1), mar = c(4, 19, 3, 3), oma = c(2, 0, 1, 0))
    mean_panel <- head(
      mean_rows[order(-mean_rows$mean_change_per_year), , drop = FALSE], n
    )
    mean_panel <- mean_panel[order(mean_panel$mean_change_per_year), , drop = FALSE]
    if (nrow(mean_panel)) {
      mean_labels <- paste0(
        mean_panel$country, "  ", mean_panel$previous_year, "-",
        mean_panel$year, ifelse(mean_panel$reliable_pair, "", " *")
      )
      positions <- graphics::barplot(
        mean_panel$mean_change_per_year,
        names.arg = mean_labels,
        horiz = TRUE,
        las = 1,
        col = "#167D8D",
        border = NA,
        xlim = c(0, max(max(mean_panel$mean_change_per_year) * 1.18, 0.1)),
        xlab = "Annualized absolute change (ESS scale points/year)",
        main = "Largest mean changes",
        cex.names = 0.75
      )
      graphics::text(
        mean_panel$mean_change_per_year, positions,
        labels = round(mean_panel$mean_change_per_year, 2), pos = 4, cex = 0.75
      )
    } else {
      graphics::plot.new()
      graphics::title(main = "Largest mean changes")
      graphics::text(0.5, 0.5, "No comparable observations")
    }

    if (nrow(iqr_rows)) {
      iqr_panel <- head(
        iqr_rows[order(-iqr_rows$iqr_change_per_year), , drop = FALSE], n
      )
      iqr_panel <- iqr_panel[order(iqr_panel$iqr_change_per_year), , drop = FALSE]
      labels <- paste0(
        iqr_panel$country, "  ", iqr_panel$previous_year, "-",
        iqr_panel$year, ifelse(iqr_panel$reliable_pair, "", " *")
      )
      y <- seq_len(nrow(iqr_panel))
      limits <- range(c(iqr_panel$previous_iqr, iqr_panel$current_iqr))
      if (diff(limits) == 0) limits <- limits + c(-0.5, 0.5)
      widened <- iqr_panel$current_iqr >= iqr_panel$previous_iqr
      colors <- ifelse(widened, "#B94D55", "#167D8D")
      graphics::plot(
        NA,
        xlim = limits,
        ylim = c(0.5, length(y) + 0.5),
        yaxt = "n",
        xlab = "ESS IQR (scale points)",
        ylab = "",
        main = "Largest IQR changes: before and after each ESS wave",
        yaxs = "i"
      )
      graphics::axis(2, at = y, labels = labels, las = 1, cex.axis = 0.75)
      graphics::segments(
        iqr_panel$previous_iqr, y, iqr_panel$current_iqr, y,
        col = colors, lwd = 2
      )
      graphics::points(iqr_panel$previous_iqr, y, pch = 21,
                       bg = "#777777", col = "#444444")
      graphics::points(iqr_panel$current_iqr, y, pch = 21,
                       bg = colors, col = colors)
    } else {
      graphics::plot.new()
      graphics::title(main = "Largest IQR changes")
      graphics::text(0.5, 0.5, "No comparable observations")
    }
    graphics::mtext(
      "* Unreliable/unknown endpoint. Grey = previous; colored = current. Red = wider IQR; teal = narrower.",
      side = 1, outer = TRUE, line = 0.5, cex = 0.8
    )
  })
}

compute_ess_summary_change <- function() {
  changes <- compute_ess_summary_changes()
  plot_files <- character()
  if (nrow(changes)) {
    plot_files <- c(
      heatmaps = file.path(paths$ess_change_figures, "change_heatmaps.png"),
      top_changes = file.path(paths$ess_change_figures, "largest_changes.png")
    )
    plot_ess_change_heatmaps(changes, plot_files[["heatmaps"]])
    plot_top_ess_changes(changes, plot_files[["top_changes"]])
  }
  list(changes = changes, plot_files = plot_files)
}

run_ess_summary_change <- function() {
  cached_result(
    "ess_summary_change",
    input_files = ess_input_files(),
    code_files = code_files_for("matching", "ess_change"),
    compute = compute_ess_summary_change,
    output_files = function(result) unname(result$plot_files)
  )
}
