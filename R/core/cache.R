code_files_for <- function(...) {
  modules <- c(...)
  project_path(c(core_files, unname(module_files[modules])))
}

fingerprint_files <- function(files) {
  info <- file.info(files)
  listing_file <- tempfile()
  on.exit(unlink(listing_file), add = TRUE)
  writeLines(paste(files, info$size, as.numeric(info$mtime)), listing_file)
  unname(tools::md5sum(listing_file))
}

# Reuses a stored result while inputs, code and generated files are unchanged.
cached_result <- function(name, input_files, code_files, compute,
                          output_files = function(value) character()) {
  key <- paste(
    c(fingerprint_files(input_files), unname(tools::md5sum(code_files))),
    collapse = "|"
  )
  cache_file <- project_path(paths$cache_dir, paste0(name, ".rds"))

  if (file.exists(cache_file)) {
    entry <- readRDS(cache_file)
    if (identical(entry$key, key) &&
        all(file.exists(project_path(entry$output_files)))) {
      return(entry$value)
    }
  }

  value <- compute()
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(key = key, value = value, output_files = output_files(value)),
    cache_file
  )
  value
}
