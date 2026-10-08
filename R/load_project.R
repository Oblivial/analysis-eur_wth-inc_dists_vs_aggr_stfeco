project_candidates <- normalizePath(
	c(getwd(), file.path(getwd(), "..", "..")),
	winslash = "/",
	mustWork = FALSE
)
project_index <- which(file.exists(file.path(
	project_candidates, "R", "load_project.R"
)))[1L]
if (is.na(project_index)) stop("Could not locate the project root.")
project_root <- project_candidates[[project_index]]

source(file.path(project_root, "R", "core", "config.R"))
project_root <- project_candidates[[project_index]]
for (file in c(core_files[-1L], unname(module_files))) {
	source(file.path(project_root, file))
}
