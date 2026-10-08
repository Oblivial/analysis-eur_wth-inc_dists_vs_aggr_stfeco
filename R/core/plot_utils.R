safe_slug <- function(text) {
  slug <- iconv(text, to = "ASCII//TRANSLIT")
  slug <- gsub("[^A-Za-z0-9]+", "_", slug)
  tolower(gsub("(^_+|_+$)", "", slug))
}

# Paths are project-relative so results stay valid if the project moves.
with_png <- function(relative_file, width, height, draw) {
  file <- project_path(relative_file)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(file, width = width, height = height, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw()
  invisible(TRUE)
}

markdown_image <- function(relative_file, alt) {
  sprintf("![%s](%s)", alt, utils::URLencode(relative_file))
}

draw_change_heatmap_panel <- function(data, value_column, title,
                                      mark_column = NULL, mark_pch = 1,
                                      marker_note = NULL,
                                      scale_label = "pp/year") {
  if (!nrow(data)) {
    graphics::plot.new()
    graphics::title(main = title)
    graphics::text(0.5, 0.5, "No comparable observations")
    return(invisible(FALSE))
  }

  data$period <- if ("round_label" %in% names(data)) {
    data$round_label
  } else {
    paste(data$previous_year, data$year, sep = "-")
  }
  period_info <- unique(data[c("previous_year", "year", "period")])
  period_info <- period_info[
    order(period_info$year, period_info$previous_year), , drop = FALSE
  ]
  period_info <- period_info[!duplicated(period_info$period), , drop = FALSE]
  periods <- period_info$period
  valid_data <- data[is.finite(data[[value_column]]), , drop = FALSE]
  if (nrow(valid_data)) {
    country_medians <- aggregate(
      valid_data[[value_column]], list(country = valid_data$country), median
    )
    active_countries <- country_medians$country[
      order(-country_medians$x, country_medians$country)
    ]
    countries <- c(
      active_countries,
      sort(setdiff(unique(data$country), active_countries))
    )
    cap <- unname(quantile(
      valid_data[[value_column]], 0.95, names = FALSE, type = 8
    ))
    if (!is.finite(cap) || cap <= 0) cap <- max(valid_data[[value_column]], 1)
  } else {
    countries <- sort(unique(data$country))
    cap <- 1
  }
  palette <- grDevices::hcl.colors(101, "YlOrRd")

  graphics::plot(
    NA,
    xlim = c(0, length(periods) + 1.2),
    ylim = c(0, length(countries)),
    xaxs = "i",
    yaxs = "i",
    axes = FALSE,
    xlab = "ESS round transition",
    ylab = "",
    main = title
  )

  for (i in seq_along(countries)) {
    row_y <- length(countries) - i
    country_data <- data[data$country == countries[[i]], , drop = FALSE]
    for (j in seq_along(periods)) {
      cell <- country_data[country_data$period == periods[[j]], , drop = FALSE]
      if (!nrow(cell) || !is.finite(cell[[value_column]][[1L]])) next
      value <- cell[[value_column]][[1L]]
      color_index <- min(101L, max(1L, floor(value / cap * 100) + 1L))
      graphics::rect(j - 1, row_y, j, row_y + 1,
                     col = palette[[color_index]], border = "white")
      if (!is.null(mark_column) && isTRUE(cell[[mark_column]][[1L]])) {
        graphics::points(j - 0.5, row_y + 0.5, pch = mark_pch,
                         cex = 0.7, lwd = 1.2, col = "#202020")
      }
    }
  }

  graphics::axis(1, at = seq_along(periods) - 0.5, labels = periods,
                 las = 2, cex.axis = 0.7)
  graphics::axis(2, at = length(countries) - seq_along(countries) + 0.5,
                 labels = countries, las = 2, cex.axis = 0.65)
  for (i in seq_len(50)) {
    lower <- length(countries) * (i - 1) / 50
    upper <- length(countries) * i / 50
    color_index <- min(101L, floor((i - 0.5) / 50 * 100) + 1L)
    graphics::rect(length(periods) + 0.35, lower,
             length(periods) + 0.55, upper,
                   col = palette[[color_index]], border = NA)
  }
  graphics::axis(
    4,
    at = length(countries) * c(0, 0.5, 1),
    labels = format(round(cap * c(0, 0.5, 1), 2), nsmall = 0),
    las = 1,
    tick = FALSE,
    cex.axis = 0.7
  )
  graphics::mtext(scale_label, side = 4, line = 3.2, cex = 0.7)
  note <- paste(
    "Color scale capped at the 95th percentile.", marker_note
  )
  graphics::mtext(note, side = 1, line = 5.2, cex = 0.72)
  invisible(TRUE)
}
