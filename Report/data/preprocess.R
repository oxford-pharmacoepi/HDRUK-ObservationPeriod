# shiny is prepared to work with this resultList, please do not change them
result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))

data <- c(
  "snapshot" = "summarise_omop_snapshot",
  "in_obs" = "summarise_in_observation",
  "obs_period" = "summarise_table",
  "incidence" = "incidence"
) |>
  purrr::map(\(x) {
    result |>
      omopgenerics::filterSettings(.data$result_type == .env$x)
  })

cdmNames <- unique(result$cdm_name)
sexes <- c("overall", "Female", "Male")
ageGroups <- c("overall", "0 to 19", "20 to 39", "40 to 59", "60 to 79", "80 or above")

data$in_obs <- data$in_obs |>
  omopgenerics::tidy() |>
  dplyr::mutate(
    year = as.integer(substr(.data$time_interval, 1, 4)),
    sex = factor(.data$sex, levels = sexes),
    age_group = factor(.data$age_group, levels = ageGroups)
  ) |>
  dplyr::select(!c("variable_level", "time_interval", "omop_table", "interval", "percentage"))

data$incidence <- data$incidence |>
  omopgenerics::splitAll() |>
  omopgenerics::addSettings() |>
  dplyr::mutate(year = dplyr::if_else(
    .data$analysis_interval == "years",
    as.integer(substr(.data$incidence_start_date, 1, 4)),
    NA_integer_
  )) |>
  dplyr::select(
    "cdm_name", outcome = "outcome_cohort_name",
    age_group = "denominator_age_group", sex = "denominator_sex",
    "analysis_interval", "year", "mode", "estimate_name", "estimate_type",
    "estimate_value"
  ) |>
  dplyr::mutate(
    age_group = dplyr::case_when(
      .data$age_group == "80 to 150" ~ "80 or above",
      .data$age_group == "0 to 150" ~ "overall",
      .default = .data$age_group
    ) |>
      factor(levels = ageGroups),
    sex = dplyr::if_else(.data$sex == "Both", "overall", .data$sex) |>
      factor(levels = sexes)
  ) |>
  dplyr::filter(!.data$estimate_name %in% c("person_days", "person_years"))

modes <- data$in_obs |>
  dplyr::distinct(.data$mode) |>
  dplyr::pull()

save(data, cdmNames, sexes, ageGroups, modes, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result)
