plot_ess_statistics <- function(country_ess, country, relative_file) {
  country_ess <- country_ess[order(country_ess$year), ]
  metrics <- c(mean = "Mean", median = "Median", mode = "Mode")
  finite_values <- unlist(country_ess[names(metrics)], use.names = FALSE)
  finite_values <- finite_values[is.finite(finite_values)]
  if (!length(finite_values)) return(FALSE)

  metric_colors <- c(mean = "#167D8D", median = "#D35F4B", mode = "#6A7F3F")
  with_png(relative_file, 1400, 850, function() {
    graphics::par(mar = c(5, 5, 4, 2))
    graphics::plot(
      NA,
      xlim = range(country_ess$year),
      ylim = range(finite_values),
      xlab = "Year",
      ylab = "Satisfaction with the state of the economy (ESS stfeco scale)",
      main = paste(country, "| ESS descriptive statistics"),
      xaxt = "n"
    )
    graphics::axis(1, at = sort(unique(country_ess$year)))

    for (metric in names(metrics)) {
      value <- country_ess[[metric]]
      reliable <- !is.na(value) & country_ess$reliable
      unreliable <- !is.na(value) & !country_ess$reliable
      graphics::lines(country_ess$year, value, col = metric_colors[[metric]], lwd = 2)
      graphics::points(country_ess$year[reliable], value[reliable],
                       pch = 16, col = metric_colors[[metric]])
      graphics::points(country_ess$year[unreliable], value[unreliable],
                       pch = 1, col = metric_colors[[metric]])
    }
    graphics::legend(
      "topleft", legend = c(unname(metrics), "Open point: flagged unreliable"),
      col = c(unname(metric_colors), "#444444"),
      lty = c(1, 1, 1, NA), pch = c(16, 16, 16, 1), bty = "n"
    )
  })
}

# Sums WID decile shares into the groups defined in share_groups.
compute_group_shares <- function(wid_concept, years) {
  group_shares <- vapply(share_groups, function(percentiles) {
    vapply(years, function(year) {
      values <- wid_concept$value[
        wid_concept$year == year & wid_concept$percentile %in% percentiles
      ]
      if (length(values) != length(percentiles) || any(!is.finite(values))) {
        return(NA_real_)
      }
      sum(values) * 100
    }, numeric(1))
  }, numeric(length(years)))

  matrix(
    group_shares,
    nrow = length(years),
    ncol = length(share_groups),
    dimnames = list(as.character(years), names(share_groups))
  )
}

plot_wid_group_shares <- function(wid_concept, country, title, years,
                                  relative_file, note = NULL) {
  years <- sort(unique(years))
  group_shares <- compute_group_shares(wid_concept, years)
  finite_values <- group_shares[is.finite(group_shares)]
  if (!length(finite_values)) return(FALSE)

  with_png(relative_file, 1500, 900, function() {
    graphics::par(mar = c(6, 5, 4, 2))
    graphics::plot(
      NA,
      xlim = range(years),
      ylim = range(finite_values),
      xlab = "Year",
      ylab = "Share of total (%)",
      main = paste(country, "|", title, "distribution over time"),
      xaxt = "n"
    )
    graphics::axis(1, at = years)
    for (i in seq_along(share_groups)) {
      graphics::lines(years, group_shares[, i], type = "o", pch = 16,
                      col = share_group_colors[[i]], lwd = 2, cex = 0.8)
    }
    graphics::legend(
      "topright", legend = names(share_groups), col = share_group_colors,
      lty = 1, pch = 16, bty = "n"
    )
    if (!is.null(note)) graphics::mtext(note, side = 1, line = 4, cex = 0.8)
  })
}

compute_country_timeseries <- function(matching) {
  wealth_note <- "Negative net wealth shares are possible for groups with debt."
  plot_files <- list()

  for (country in names(matching$matches)) {
    match <- matching$matches[[country]]
    country_ess <- matching$ess[matching$ess$country == country, ]
    country_ess <- country_ess[country_ess$year %in% match$years, ]
    country_dir <- file.path(paths$timeseries_figures, country)
    slug <- safe_slug(country)
    files <- c(
      ess = file.path(country_dir, paste0(slug, "_ess.png")),
      wealth = file.path(country_dir, paste0(slug, "_wealth_shares.png")),
      income = file.path(country_dir, paste0(slug, "_income_shares.png"))
    )

    drawn <- c(
      plot_ess_statistics(country_ess, country, files[["ess"]]),
      plot_wid_group_shares(
        match$wid[match$wid$concept == "wealth", ], country,
        wid_concepts[["wealth"]], match$years, files[["wealth"]],
        note = wealth_note
      ),
      plot_wid_group_shares(
        match$wid[match$wid$concept == "income", ], country,
        match$income_label, match$years, files[["income"]]
      )
    )
    if (all(drawn)) plot_files[[country]] <- files
  }

  list(plot_files = plot_files)
}

run_country_timeseries <- function(matching) {
  cached_result(
    "country_timeseries",
    input_files = raw_input_files(),
    code_files = code_files_for("matching", "timeseries"),
    compute = function() compute_country_timeseries(matching),
    output_files = function(result) unlist(result$plot_files, use.names = FALSE)
  )
}

render_country_timeseries <- function(timeseries) {
  captions <- c(
    ess = "ESS mean, median, and mode",
    wealth = "Net personal wealth distribution",
    income = "WID national income distribution"
  )
  for (country in names(timeseries$plot_files)) {
    cat("\n### ", country, "\n\n", sep = "")
    for (kind in names(captions)) {
      cat(
        "#### ", captions[[kind]], "\n\n",
        markdown_image(
          timeseries$plot_files[[country]][[kind]],
          paste(country, captions[[kind]])
        ),
        "\n\n",
        sep = ""
      )
    }
  }
}
