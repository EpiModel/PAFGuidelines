source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

if (!fs::dir_exists("out/tables"))
  fs::dir_create("out/tables")

if (!fs::dir_exists("out/tables_data"))
  fs::dir_create("out/tables_data")

scenarios_names <- names(scenarios)

# T1
scenarios <- scenarios_names[grepl("^t1_.*", scenarios_names)]
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t1.rds")
readr::write_csv(d_table, "out/tables/t1.csv")

# T2
scenarios <- scenarios_names[grepl("^t2_.*", scenarios_names)]
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t2.rds")
readr::write_csv(d_table, "out/tables/t2.csv")

# T3
scenarios <- scenarios_names[grepl("^t3_.*", scenarios_names)]
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t3.rds")
readr::write_csv(d_table, "out/tables/t3.csv")
