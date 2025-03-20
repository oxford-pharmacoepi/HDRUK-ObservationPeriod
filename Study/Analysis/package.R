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
}
