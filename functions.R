createLog <- function() {
  fileName <- here::here("Results", format(Sys.time(), "log_%Y_%m_%d_%H_%M_%S.txt"))
  options(omopgenerics_logger = fileName)
  logMessage("Logger created")
}
logMessage <- function(message) {
  date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cli::cli_inform(paste0("{.pkg ", date, "} ", message))
  con <- file(getOption("omopgenerics_logger"), open = "a")
  writeLines(paste0(date, " - ", message), con)
  close(con)
  invisible(NULL)
}
generateMinDateObservationPeriod <- function(cdm, dataEndDate, censorAge) {
  # initial validation
  dataEndDate <- as.Date(dataEndDate)
  omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertDate(dataEndDate, length = 1)

  tables <- c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  )
  nm1 <- omopgenerics::uniqueTableName()
  nm2 <- omopgenerics::uniqueTableName()
  for (k in seq_along(tables)) {
    table <- tables[k]
    startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
    xk <- cdm[[table]] |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarise(
        start_date = min(.data[[startDate]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::compute(name = nm1)
    if (k > 1) {
      x <- x |>
        dplyr::full_join(xk, by = "person_id") |>
        dplyr::mutate(min_date = dplyr::case_when(
          is.na(.data$min_date) ~ .data$start_date,
          is.na(.data$start_date) ~ .data$min_date,
          .data$min_date < .data$start_date ~ .data$min_date,
          .default = .data$start_date
        )) |>
        dplyr::select("person_id", "min_date") |>
        dplyr::compute(name = nm2)
    } else {
      x <- xk |>
        dplyr::rename("min_date" = "start_date") |>
        dplyr::compute(name = nm2)
    }
  }
  omopgenerics::dropSourceTable(cdm = cdm, name = nm1)

  cdm$observation_period <- x |>
    PatientProfiles::addDateOfBirth(name = nm2) |>
    dplyr::rename(observation_period_start_date = "min_date") |>
    dplyr::mutate(
      date_age = clock::add_years(.data$date_of_birth, !!as.integer(censorAge)),
      observation_period_end_date = dplyr::if_else(
        .data$date_age <= .env$dataEndDate, .data$date_age, .env$dataEndDate
      ),
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::left_join(
      cdm$death |>
        dplyr::group_by(.data$person_id) |>
        dplyr::summarise(death_date = min(.data$death_date, na.rm = TRUE)),
      by = "person_id"
    ) |>
    dplyr::mutate(observation_period_end_date = dplyr::case_when(
      is.na(.data$death_date) ~ .data$observation_period_end_date,
      .data$death_date < .data$observation_period_end_date ~ .data$death_date,
      .default = .data$observation_period_end_date
    )) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")
  omopgenerics::dropSourceTable(cdm = cdm, name = nm2)

  return(cdm)
}
generateMinMaxObservationPeriod <- function(cdm) {
  # initial validation
  omopgenerics::validateCdmArgument(cdm = cdm)

  tables <- c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  )
  nm1 <- omopgenerics::uniqueTableName()
  nm2 <- omopgenerics::uniqueTableName()
  for (k in seq_along(tables)) {
    table <- tables[k]
    startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
    endDate <- omopgenerics::omopColumns(table = table, field = "end_date")
    xk <- cdm[[table]] |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarise(
        start_date = min(.data[[startDate]], na.rm = TRUE),
        end_date = max(.data[[endDate]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::compute(name = nm1)
    if (k > 1) {
      x <- x |>
        dplyr::full_join(xk, by = "person_id") |>
        dplyr::mutate(
          min_date = dplyr::case_when(
            is.na(.data$min_date) ~ .data$start_date,
            is.na(.data$start_date) ~ .data$min_date,
            .data$min_date < .data$start_date ~ .data$min_date,
            .default = .data$start_date
          ),
          max_date = dplyr::case_when(
            is.na(.data$max_date) ~ .data$end_date,
            is.na(.data$end_date) ~ .data$max_date,
            .data$max_date > .data$end_date ~ .data$max_date,
            .default = .data$end_date
          )
        ) |>
        dplyr::select("person_id", "min_date", "max_date") |>
        dplyr::compute(name = nm2)
    } else {
      x <- xk |>
        dplyr::rename("min_date" = "start_date", "max_date" = "end_date") |>
        dplyr::compute(name = nm2)
    }
  }
  omopgenerics::dropSourceTable(cdm = cdm, name = nm1)

  cdm$observation_period <- x |>
    dplyr::rename(
      observation_period_start_date = "min_date",
      observation_period_end_date = "max_date"
    ) |>
    dplyr::mutate(
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")
  omopgenerics::dropSourceTable(cdm = cdm, name = nm2)

  return(cdm)
}
generateVisitObservationPeriod <- function(cdm) {
  # initial validation
  omopgenerics::validateCdmArgument(cdm = cdm)

  tables <- c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  )
  for (k in seq_along(tables)) {
    table <- tables[k]
    startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
    endDate <- omopgenerics::omopColumns(table = table, field = "end_date")
    xk <- cdm[[table]] |>
      dplyr::select(
        "person_id",
        "start_date" = dplyr::all_of(startDate),
        "end_date" = dplyr::all_of(endDate)
      )
    if (k > 1) {
      x <- x |>
        dplyr::union_all(xk)
    } else {
      x <- xk
    }
  }
  logMessage("join overlap")
  x <- x |>
    dplyr::mutate(end_date = dplyr::case_when(
      is.na(.data$end_date) ~ .data$start_date,
      .data$end_date < .data$start_date ~ .data$start_date,
      .default = .data$end_date
    )) |>
    CohortConstructor:::joinOverlap(
      name = "observation_period",
      gap = 0L,
      startDate = "start_date",
      endDate = "end_date",
      by = "person_id"
    )

  logMessage("final summary")
  cdm$observation_period <- x |>
    dplyr::rename(
      observation_period_start_date = "start_date",
      observation_period_end_date = "end_date"
    ) |>
    dplyr::mutate(
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")
  omopgenerics::dropSourceTable(cdm = cdm, name = nm2)

  return(cdm)
}
erafyObservationPeriod <- function(cdm, gap = 0L) {
  cdm$observation_period <- cdm$observation_period |>
    dplyr::select(
      "person_id", "observation_period_start_date",
      "observation_period_end_date"
    ) |>
    CohortConstructor:::joinOverlap(
      name = "observation_period",
      gap = gap,
      startDate = "observation_period_start_date",
      endDate = "observation_period_end_date",
      by = "person_id"
    ) |>
    dplyr::mutate(
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")
  return(cdm)
}
summaryInObservation <- function(cdm, mode) {
  ageGroup <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf))
  logMessage("summarise in observation")
  res1 <- OmopSketch::summariseInObservation(
    cdm$observation_period,
    interval = "years",
    output = c("records", "person-days"),
    ageGroup = ageGroup,
    sex = TRUE
  )
  logMessage("summarise observation metrics")
  res2 <- OmopSketch::summariseObservationPeriod(
    observationPeriod = cdm$observation_period,
    estimates = c("mean", "sd", "min", "q25", "median", "q75", "max", "density"),
    ageGroup = ageGroup,
    sex = TRUE
  ) |>
    dplyr::filter(
      !stringr::str_detect(.data$estimate_name, "density") ||
        .data$group_level == "all"
    )
  logMessage("bind result")
  result <- omopgenerics::bind(res1, res2)
  result |>
    omopgenerics::newSummarisedResult(
      settings = omopgenerics::settings(result) |>
        dplyr::mutate(mode = .env$mode)
    )
}
