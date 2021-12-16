source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

if (!fs::dir_exists("out/tables")) {
  fs::dir_create("out/tables")
}

if (!fs::dir_exists("out/tables_data")) {
  fs::dir_create("out/tables_data")
}

scenarios_names <- names(scenarios)
scenarios <- scenarios_names[grepl("^t1_.*", scenarios_names)]
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

baseline_file <- scenarios_files[1]

fle <- scenarios_files[[2]]
