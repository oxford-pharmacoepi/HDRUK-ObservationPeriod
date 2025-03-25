fileData <- file.path(getwd(), "data", "shinyData.RData")
if (!file.exists(fileData)) {
  # preprocess data if it has not been done
  source(file.path(getwd(), "data", "preprocess.R"))
} else {
  load(fileData)
}

characteristics <- data$obs_period |>
  dplyr::mutate(
    estimate_value = dplyr::if_else(
      stringr::str_detect(.data$variable_name, "_per_day"),
      as.character(1000 * as.numeric(.data$estimate_value)),
      .data$estimate_value
    )
  ) |>
  visOmopResults::formatEstimateValue(decimals = c(numeric = 0, integer = 0, percentage = 1)) |>
  visOmopResults::formatEstimateName(estimateName = c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "Range" = "<min> to <max>"
  )) |>
  omopgenerics::splitAll() |>
  omopgenerics::addSettings(settingsColumn = "mode") |>
  dplyr::mutate(
    variable_name = dplyr::if_else(
      .data$variable_name == "sex" & .data$variable_level == "Female",
      "Female", .data$variable_name
    )
  ) |>
  dplyr::filter(
    !.data$estimate_name %in% c("mean", "sd", "density_x", "density_y"),
    !.data$variable_name %in% c(
      "sex", "age_group_start", "age_group_end", "number_drugs",
      "number_conditions", "number_visits"
    ),
    .data$estimate_name != "Range" | .data$variable_name %in% c(
      "duration", "time_to_next_observation", "op_per_person"
    )
  ) |>
  dplyr::select(!c("result_id", "variable_level")) |>
  dplyr::left_join(
    dplyr::tribble(
      ~variable_name, ~new_variable_name,
      "number records", "Number records",
      "number subjects", "Number subjects",
      "duration", "OP length",
      "time_to_next_observation", "Time to next OP",
      "op_per_person", "OP per person",
      "age_start", "Age (start)",
      "age_end", "Age (end)",
      "Female", "Female",
      "drugs_per_day", "Drugs per 1,000 days",
      "conditions_per_day", "Conditions per 1,000 days",
      "visits_per_day", "Visits per 1,000 days"
    ) |>
      dplyr::mutate(order_id = dplyr::row_number()),
    by = "variable_name"
  ) |>
  dplyr::mutate(new_variable_name = dplyr::coalesce(.data$new_variable_name, .data$variable_name)) |>
  dplyr::arrange(.data$order_id) |>
  dplyr::select(!c("variable_name", "order_id")) |>
  dplyr::rename(variable_name = "new_variable_name") |>
  dplyr::relocate("variable_name")

visOmopResults::visTable(
  result = characteristics,
  header = "mode",
  groupColumn = "cdm_name",
  hide = "estimate_type"
)

x <- data$in_obs |>
  dplyr::filter(
    .data$variable_name == "Number person-days",
    .data$age_group %in% c("overall", "0 to 19", "80 or above"),
    .data$sex == "overall",
    .data$year >= 2012L
  ) |>
  dplyr::group_by(.data$mode, .data$age_group, .data$cdm_name) |>
  dplyr::summarise(total = sum(.data$count), .groups = "drop") |>
  dplyr::group_by(.data$mode) |>
  dplyr::mutate(id = dplyr::cur_group_id()) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(.data$total))

ggplot2::ggplot(data = x, mapping = ggplot2::aes(x = mode, y = total, colour = age_group)) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(cdm_name ~ ., scales = "free")
