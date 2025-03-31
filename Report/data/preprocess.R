# shiny is prepared to work with this resultList, please do not change them
result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))

mode <- dplyr::tribble(
  ~mode, ~new_mode,
  "original_data", "Original",
  "min_extract", "First record to extract",
  "min_max", "First to last record",
  "impatient", "Inpatient hospitalisation",
  "persistence_0_surveillanceFALSE", "Persistence 0",
  "persistence_180_surveillanceFALSE", "Persistence 180",
  "persistence_180_surveillanceTRUE", "Persistence 180 + surveillance",
  "persistence_365_surveillanceFALSE", "Persistence 365",
  "persistence_365_surveillanceTRUE", "Persistence 365 + surveillance",
  "persistence_545_surveillanceFALSE", "Persistence 545",
  "persistence_545_surveillanceTRUE", "Persistence 545 + surveillance",
  "persistence_730_surveillanceFALSE", "Persistence 730",
  "persistence_730_surveillanceTRUE", "Persistence 730 + surveillance"
)

cdmNames <- dplyr::tribble(
  ~cdm_name, ~new_cdm_name,
  "Barts Health", "Barts Health",
  "CPRD AURUM 202409 - 50 prac", "CPRD AURUM 50",
  "GOSH_OMOP", "GOSH",
  "IDRIL_1", "IDRIL",
  "UCLH-from-2019", "UCLH"
)

result <- result |>
  dplyr::inner_join(cdmNames, by = "cdm_name") |>
  dplyr::select(!"cdm_name") |>
  dplyr::rename("cdm_name" = "new_cdm_name") |>
  omopgenerics::newSummarisedResult(
    settings = omopgenerics::settings(result) |>
      dplyr::inner_join(mode, by = "mode") |>
      dplyr::select(!"mode") |>
      dplyr::rename("mode" = "new_mode")
  )

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
