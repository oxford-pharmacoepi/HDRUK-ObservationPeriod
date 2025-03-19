library(magrittr)

# source functions and create logger
source(here::here("Analysis", "functions.R"))
createLog()

dataEndDate <- as.Date(dataEndDate)
omopgenerics::assertDate(dataEndDate, length = 1)

# create cdm object
logMessage("Create cdm object")
cdm <- CDMConnector::cdmFromCon(
  con = con, cdmSchema = cdmSchema, writeSchema = writeSchema,
  writePrefix = writePrefix, cdmName = dbName
)

# snapshot
logMessage("Extract cdm snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm)

# instantiate antibiotics cohorts
logMessage("Instantiate antibiotics cohorts")
codelist <- CodelistGenerator::getDrugIngredientCodes(
  cdm = cdm,
  name = c("azithromycin", "ciprofloxacin", "teicoplanin"),
  nameStyle = "{concept_name}"
)
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm, conceptSet = codelist, name = "outcome"
)

# original observation period
logMessage("Characterise original observation period")
originalResult <- summaryInObservation(cdm, "original_data")

# min-extract observation period
logMessage("Create min-extract observation period")
cdm <- generateMinDateObservationPeriod(cdm, dataEndDate, censorAge)
logMessage("Characterise min-extract observation period")
minExtractResult <- summaryInObservation(cdm, "min_extract")

# min-max observation period
logMessage("Create min-max observation period")
cdm <- generateMinMaxObservationPeriod(cdm)
logMessage("Characterise min-max observation period")
minMaxResult <- summaryInObservation(cdm, "min_max")

logMessage("Create visit observation period")
cdm <- generateVisitObservationPeriod(cdm, "otest")

combinations <- dplyr::tibble(persistence = 0L, surveillance = FALSE) |>
  dplyr::union_all(tidyr::expand_grid(
    persistence = c(180L, 365L, 545L, 730L),
    surveillance = c(TRUE, FALSE)
  ))
resultPersistenceSurveillance <- combinations |>
  purrr::pmap(\(persistence, surveillance) {
    name_id <- paste0("persistence_", persistence, "_surveillance", surveillance)
    logMessage(paste("Create observation period:", persistence, surveillance))
    cdm <- generateObservationPeriod(cdm = cdm,
                                     oname = "otest",
                                     persistence = persistence,
                                     surveillance = surveillance)

    logMessage("Characterise visit observation period")
    summaryInObservation(cdm, name_id)
  }) |>
  omopgenerics::bind()

# export data
logMessage("Export results")
omopgenerics::exportSummarisedResult(
  snapshot,
  originalResult,
  minExtractResult,
  minMaxResult,
  resultPersistenceSurveillance,
  minCellCount = minCellCount,
  fileName = "observation_period_characterisation_{cdm_name}",
  path = here::here("Results")
)
