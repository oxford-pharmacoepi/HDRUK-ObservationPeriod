generateObservationPeriod <- function(cdm,
                                      collapseEra = Inf,
                                      persistenceWindow = Inf,
                                      censorDate = Sys.time(),
                                      censorAge = 150,
                                      recordsFrom = c(
                                        "drug_exposure", "condition_occurrence",
                                        "procedure_occurrence",
                                        "visit_occurrence", "device_exposure",
                                        "measurement", "observation", "death"
                                      )) {
  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertNumeric(collapseEra, length = 1)
  omopgenerics::assertNumeric(persistenceWindow, length = 1)
  censorDate <- as.Date(censorDate)
  omopgenerics::assertDate(censorDate, length = 1)
  omopgenerics::assertNumeric(censorAge, length = 1)
  omopgenerics::assertChoice(recordsFrom, choices = c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  ))
  recordsFrom <- unique(recordsFrom)
  if (!is.infinite(censorAge)) censorAge <- as.integer(censorAge)

  if (length(recordsFrom) == 0) {
    # return empty
  }

  name <- omopgenerics::uniqueTableName()
  x <- getTemptativeDates(
    cdm = cdm, tables = recordsFrom, collapse = collapseEra,
    window = persistenceWindow, name = name
  )

}
getTemptativeDates <- function(cdm, tables, collapse, window, name) {
  if (is.infinite(collapseEra)) {
    if (is.infinite(persistenceWindow)) {
      end <- FALSE
    } else {
      end <- TRUE
    }
    q <- c(
      "min(as.Date(.data[['{startDate}']]), na.rm = TRUE)",
      "min(dplyr::coalesce(as.Date(.data[['{endDate}']]), as.Date(.data[['{startDate}']])), na.rm = TRUE)"
    ) |>
      rlang::set_names(c("start_date", "end_date"))
    q <- q[c(TRUE, end)]

  } else {

  }
}
