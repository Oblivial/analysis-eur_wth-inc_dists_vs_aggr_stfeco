# Draw a project-relative PNG and close its graphics device even if drawing fails.
with_png <- function(relative_file, width, height, draw) {
  file <- project_path(relative_file)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(file, width = width, height = height, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw()
  invisible(TRUE)
}

# Fill an unavailable plot panel with its title and an explanatory message.
empty_panel <- function(title, message = "No comparable observations") {
  graphics::plot.new()
  graphics::title(main = title)
  graphics::text(0.5, 0.5, message)
}

# Linear map onto cex 0.7-2.7; a flat range gives a constant medium size.
scale_point_size <- function(values, limits = range(values, finite = TRUE)) {
  span <- diff(limits)
  if (!is.finite(span) || span <= 0) return(rep(1.5, length(values)))
  0.7 + 2 * (values - limits[[1L]]) / span
}
