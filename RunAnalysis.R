
# source functions and create logger
source(here::here("functions.R"))
createLog()

dataEndDate <- as.Date(dataEndDate)
omopgenerics::assertDate(dataEndDate, length = 1)

# create cdm object
logMessage("Create cdm object")
cdm <- CDMConnector::cdmFromCon(
  con = con, cdmSchema = cdmSchema, writeSchema = writeSchema,
  writePrefix = writePrefix, cdmName = dbName
)

# original observation period
logMessage("Characterise original observation period")
originalResult <- summaryInObservation(cdm, "original_data")

# min-extract observation period
logMessage("Create min-extract observation period")
cdm <- generateMinDateObservationPeriod(cdm)
logMessage("Characterise min-extract observation period")
minExtractResult <- summaryInObservation(cdm, "min_extract")

# min-max observation period
logMessage("Create min-max observation period")
cdm <- generateMinMaxObservationPeriod(cdm)
logMessage("Characterise min-max observation period")
minMaxResult <- summaryInObservation(cdm, "min_max")

# visit observation period
logMessage("Create visit observation period")
cdm <- generateVisitObservationPeriod(cdm)
logMessage("Characterise visit observation period")
visitResult <- summaryInObservation(cdm, "visit_0_gap")

# 180 visit observation period
logMessage("Erafy 180 visit observation period")
cdm <- erafyObservationPeriod(cdm = cdm, gap = 180)
logMessage("Characterise 180 visit observation period")
visit180Result <- summaryInObservation(cdm, "visit_180_gap")

# 365 visit observation period
logMessage("Erafy 365 visit observation period")
cdm <- erafyObservationPeriod(cdm = cdm, gap = 365)
logMessage("Characterise 365 visit observation period")
visit365Result <- summaryInObservation(cdm, "visit_365_gap")

# 730 visit observation period
logMessage("Erafy 730 visit observation period")
cdm <- erafyObservationPeriod(cdm = cdm, gap = 730)
logMessage("Characterise 730 visit observation period")
visit730Result <- summaryInObservation(cdm, "visit_730_gap")

# export data
logMessage("Export results")
omopgenerics::exportSummarisedResult(
  originalResult,
  minExtractResult,
  minMaxResult,
  visitResult,
  visit180Result,
  visit365Result,
  visit730Result,
  minCellCount = minCellCount,
  fileName = "observation_period_characterisation_{cdm_name}",
  path = here::here("Results")
)
