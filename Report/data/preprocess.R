# shiny is prepared to work with this resultList, please do not change them
result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))

data <- c(
  "snapshot" = "summarise_omop_snapshot",
  "in_obs" = "summarise_in_observation",
  "obs_period" = "summarise_observation_period"
) |>
  purrr::map(\(x) {
    result |>
      omopgenerics::filterSettings(.data$result_type == .env$x) |>
      omopgenerics::tidy()
  })

save(data, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result)
