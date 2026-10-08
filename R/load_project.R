source("R/core/config.R")
for (file in c(core_files[-1L], unname(module_files))) source(file)
