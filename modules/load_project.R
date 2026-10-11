project_root <- local({
  candidate <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(candidate, "modules", "load_project.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not locate the project root.")
    candidate <- parent
  }
  candidate
})

# Resolve paths relative to the detected project root, independent of the caller's directory.
project_path <- function(...) file.path(project_root, ...)

source(project_path("modules", "config.R"))
for (file in c(core_files[-1L], module_files)) source(project_path(file))
