-----
header: "Observation period study"
-----

## Observation period exploration study

`observation_period` is a key table in the OMOP CDM as it determines when individuals are in observation in the database. For GP where there are clear enrollment periods, observation is easy to define and any patient that is enrolled is supposed to be 'in observation'. When working with Hospital records there is no enrollment dates and subjects can come back to the same subject or never seen again in the hospital. This makes it tricky to determine when individuals are in observation.

In this study we compare different definitions of the observation period using the available data of different Hospital databases.

Definitions:

- **original_data**: The original observation period in the database.
- **min_extract**: All individuals contribute from their first record till data extraction date.
- **min_max**: All individuals contribute from their first record till their last record.
- **percistence_0**: Individuals contribute while they have an ongoing record.
- **persistence_180_surveillanceFALSE**: contribute while they have an ongoing record. Two consecutive periods separated by less than 180 days are merged in a continuous observation episode.
- **persistence_180_surveillanceTRUE**: contribute while they have an ongoing record. Two consecutive periods separated by less than 180 days are merged in a continuous observation episode. A period of 180 days of surveillance is added after any observation episode.
- **persistence_365_surveillanceFALSE**: contribute while they have an ongoing record. Two consecutive periods separated by less than 365 days are merged in a continuous observation episode.
- **persistence_365_surveillanceTRUE**: contribute while they have an ongoing record. Two consecutive periods separated by less than 365 days are merged in a continuous observation episode. A period of 365 days of surveillance is added after any observation episode.
- **persistence_545_surveillanceFALSE**: contribute while they have an ongoing record. Two consecutive periods separated by less than 545 days are merged in a continuous observation episode.
- **persistence_545_surveillanceTRUE**: contribute while they have an ongoing record. Two consecutive periods separated by less than 545 days are merged in a continuous observation episode. A period of 545 days of surveillance is added after any observation episode.
- **persistence_730_surveillanceFALSE**: contribute while they have an ongoing record. Two consecutive periods separated by less than 730 days are merged in a continuous observation episode.
- **persistence_730_surveillanceTRUE**: contribute while they have an ongoing record. Two consecutive periods separated by less than 730 days are merged in a continuous observation episode. A period of 730 days of surveillance is added after any observation episode.

NOTE 1: records can be any of the following tables: visit_occurrence, condition_occurrence, drug_exposure, device_exposure, procedure_occurrence, measurement, observation and death; start and end dates are used to build the synthetic observation periods.

NOTE 2: all observation periods are trimmed to end of data extraction or record of death.

![](ohdsi_logo.svg){width=50px}

