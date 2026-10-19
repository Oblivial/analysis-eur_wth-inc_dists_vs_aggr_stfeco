welfare_regime_levels <- c(
  "Liberal", "Conservative", "Socialist", "Hybrid1", "Hybrid2",
  "Post-Communist", "Southern European", "Social Democratic", "Other/Non-EU",
  "Not classified"
)

post_communist_levels <- c("Post-Communist", "Not Post-Communist", "Not classified")

attach_welfare_regimes <- function(
    data, typology = read.csv(
      project_path(paths$esping_typology_csv), stringsAsFactors = FALSE
    )) {
  if (!all(c("Country", "EA_Type") %in% names(typology))) {
    stop("Welfare typology must contain Country and EA_Type columns.")
  }
  if (anyNA(typology$Country) || any(!nzchar(typology$Country)) ||
      anyDuplicated(typology$Country) || anyNA(typology$EA_Type) ||
      any(!typology$EA_Type %in% setdiff(welfare_regime_levels, "Not classified"))) {
    stop("Welfare typology must contain unique countries and supported regime labels.")
  }
  data$welfare_regime <- typology$EA_Type[match(data$country, typology$Country)]
  missing <- is.na(data$welfare_regime)
  if (any(missing)) {
    warning(
      "Missing welfare-regime classification; retaining as Not classified: ",
      paste(unique(data$country[missing]), collapse = ", "),
      call. = FALSE
    )
    data$welfare_regime[missing] <- "Not classified"
  }
  data$post_communist_status <- ifelse(
    data$welfare_regime == "Not classified", "Not classified",
    ifelse(data$welfare_regime == "Post-Communist", "Post-Communist", "Not Post-Communist")
  )
  data
}
