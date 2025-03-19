
# restore the renv environment
renv::activate()
renv::restore()

# create connection
server_dbi <- "cdm_gold_202407"
port<-Sys.getenv("DB_PORT")
host<-Sys.getenv("DB_HOST")
user<-Sys.getenv("DB_USER")
password<-Sys.getenv("DB_PASSWORD")
con <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = server_dbi,
                       port = port,
                       host = host,
                       user = user,
                       password = password)

# database acronym (e.g. "CPRD AURUM")
dbName <- "CPRD GOLD 100k"

# schemas and prefix
cdmSchema <- "public_100k"
writeSchema <- "results"
writePrefix <- "mcs_"

# suppression
minCellCount <- 5L

# data cut
dataEndDate <- "2024-07-01"
censorAge <- 150L

source(here::here("RunAnalysis.R"))
