# Reproducible analysis environment

This project uses [`renv`](https://rstudio.github.io/renv/) to pin its R package
dependencies in `renv.lock`.

## Setup

Install R 4.5.1 or a compatible R 4.5 release and Quarto. From the project root,
run:

```r
install.packages("renv")
renv::restore()
```

The project `.Rprofile` activates `renv` automatically. `renv::restore()` installs
the locked analysis and report packages into this project's private library.

## Run

Render the report and run the analysis contract checks from the project root:

```sh
quarto render index.qmd
Rscript tests/check_analysis_contracts.R
```

After changing package dependencies, install them into the project library and
update the lockfile with `renv::snapshot()`.
