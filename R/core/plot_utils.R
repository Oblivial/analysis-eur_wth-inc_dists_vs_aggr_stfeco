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

