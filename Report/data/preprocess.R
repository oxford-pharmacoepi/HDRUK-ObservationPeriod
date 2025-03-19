# shiny is prepared to work with this resultList, please do not change them
result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))

data <- c(
  "snapshot" = "summarise_omop_snapshot",
  "in_obs" = "summarise_in_observation",
  "obs_period" = "summarise_observation_period"
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

modes <- data$in_obs |>
  dplyr::distinct(.data$mode) |>
  dplyr::pull()

save(data, cdmNames, sexes, ageGroups, modes, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result)
